package table

import oracle.jdbc.OracleConnection
import schema.ComparePlan
import table.DiffApplier.bindRow
import tui.ReportMsg.noticeWithLog
import tui.SyncTUI.bullet
import tui.TaskStatus.{Fin, InProc}
import tui.layoutzEx.StringWithColor
import tui.{ReportMsg, ReportMsgAbort, ReportMsgAp, ReportMsgApCancel, ReportMsgApSkip}
import utils.LogHelper.{memo, oops}

import java.io.{BufferedReader, InputStream, InputStreamReader, StringReader}
import java.sql.{Connection, PreparedStatement, Statement, Types}

case class DiffApplier( plan: ComparePlan,
                        partialCommit: Boolean = true,
                        commitSize: Int = 1024,
                        debug: Boolean = false,
                        batchSize: Int = 512 ) {


  def applyChange(diffs: Iterator[(Int, DiffRow)],
                  conn: Connection,
                  notice: ReportMsg => Unit,
                  cancel: () => Boolean,
                  outDiffNums: Option[() => java.io.OutputStream] = None) = {

    def onCancel(cp: Int) = noticeWithLog(notice){
      ReportMsgApCancel(plan.name, cp)
    }

    def noticeSkip(offset: Int, msg: String): Unit = noticeWithLog(notice){
      ReportMsgApSkip(plan.name, offset, msg)
    }

    def reportAp(ic: Long, uc: Long, dc: Long, sc: Long,
                 si: Long, su: Long, sd: Long, fin: Boolean) = noticeWithLog(notice){
      val status = if(fin) Fin else InProc
      ReportMsgAp(plan.name, ic, uc, dc, sc, si, su, sd, status)
    }

    val iStmt = conn.prepareStatement(plan.insertSql)
    val uStmt = conn.prepareStatement(plan.updateSql)
    val dStmt = conn.prepareStatement(plan.deleteSql)

    conn.setAutoCommit(false)

    var ic, uc, dc, sc = 0L
    var si, su, sd = 0L

    var processedSinceCommit = 0L
    var lastRowNum = -1

    val outStream = outDiffNums.map(open => open())

    def closeOutStream(): Unit = {
      outStream.foreach { s =>
        try {
          s.flush(); s.close()
        } catch {
          case _: Exception =>
        }
      }
    }

    def executeBatchSafe(stmt: PreparedStatement, stmtType: String, diffNums: Seq[Int]): Unit = {

      def processResults(results: Array[Int]): Unit = {
        results.zip(diffNums).foreach { case (cnt, num) =>

            // 1. 실제로 1개 이상의 행이 영향을 받았거나, 성공했지만 개수 정보가 없는 경우 (-2)
            if (cnt > 0 || cnt == Statement.SUCCESS_NO_INFO) {
              stmtType match {
                case "INS" => ic += 1
                case "UPD" => uc += 1
                case "DEL" => dc += 1
              }
            }
            // 2. 실행에 실패했거나 (-3), 성공은 했으나 실제 반영된 행이 0건인 경우
            else {
              stmtType match {
                case "INS" => si += 1
                case "UPD" => su += 1
                case "DEL" => sd += 1
              }
              sc += 1
              noticeSkip(num, s"$stmtType (affected: $cnt)")
              outStream.foreach(_.write((num.toString + "\n").getBytes("UTF-8")))
            }
        }
      }
      try {
        val results = stmt.executeBatch()
        processResults(results)
      } catch {
        case e: java.sql.BatchUpdateException => processResults(e.getUpdateCounts)
      }
      processedSinceCommit += diffNums.size
      if (partialCommit && processedSinceCommit >= commitSize) {
        conn.commit()
        processedSinceCommit = 0L
      }
    }

    try {
      val valIndices = plan.valIndices
      val keyIndices = plan.keyIndices
      val hasLongColumn = valIndices.exists(i =>
        i._2 == java.sql.Types.LONGVARCHAR || i._2 == java.sql.Types.LONGNVARCHAR
      )

      val iNums = scala.collection.mutable.ArrayBuffer[Int]()
      val uNums = scala.collection.mutable.ArrayBuffer[Int]()
      val dNums = scala.collection.mutable.ArrayBuffer[Int]()

      val it = diffs.iterator
      while (it.hasNext && !cancel()) {
        val (n, diff) = it.next()
        lastRowNum = n
        diff match {
          case OnlyInA(keys, sourceVals) =>
            val ai = keyIndices ++ valIndices
            bindRow(iStmt, keys ++ sourceVals, ai, debug)
            if (hasLongColumn) {
              try {
                val affected = iStmt.executeUpdate()
                if (affected > 0) ic += affected else {
                  sc += 1; si += 1; noticeSkip(n, "INS")
                  outStream.foreach(_.write((n.toString + "\n").getBytes("UTF-8")))
                }
              } catch {
                case _: Exception =>
                  sc += 1; si += 1; noticeSkip(n, "INS")
                  outStream.foreach(_.write((n.toString + "\n").getBytes("UTF-8")))
              }
            } else {
              iStmt.addBatch()
              iNums += n
              if (iNums.size >= batchSize) {
                executeBatchSafe(iStmt, "INS", iNums.toSeq)
                iNums.clear()
                reportAp(ic, uc, dc, sc, si, su, sd, false)
              }
            }

          case Update(keys, sourceVals) =>
            val ui = valIndices ++ keyIndices
            bindRow(uStmt, sourceVals ++ keys, ui, debug)
            if (hasLongColumn) {
              try {
                val affected = uStmt.executeUpdate()
                if (affected > 0) uc += affected else {
                  sc += 1; su += 1; noticeSkip(n, "UPD")
                  outStream.foreach(_.write((n.toString + "\n").getBytes("UTF-8")))
                }
              } catch {
                case _: Exception =>
                  sc += 1; su += 1; noticeSkip(n, "UPD")
                  outStream.foreach(_.write((n.toString + "\n").getBytes("UTF-8")))
              }
            } else {
              uStmt.addBatch()
              uNums += n
              if (uNums.size >= batchSize) {
                executeBatchSafe(uStmt, "UPD", uNums.toSeq)
                uNums.clear()
                reportAp(ic, uc, dc, sc, si, su, sd, false)
              }
            }

          case OnlyInB(keys) =>
            val di = keyIndices
            bindRow(dStmt, keys, di, debug)
            dStmt.addBatch()
            dNums += n
            if (dNums.size >= batchSize) {
              executeBatchSafe(dStmt, "DEL", dNums.toSeq)
              dNums.clear()
              reportAp(ic, uc, dc, sc, si, su, sd, false)
            }

          case Same(_) =>
            sc += 1; noticeSkip(n, "SAME")
            if (sc % batchSize == 0) {
              reportAp(ic, uc, dc, sc, si, su, sd, false)
            }
        }
      }

      if (cancel()) {
        if (iNums.nonEmpty) executeBatchSafe(iStmt, "INS", iNums.toSeq)
        if (uNums.nonEmpty) executeBatchSafe(uStmt, "UPD", uNums.toSeq)
        if (dNums.nonEmpty) executeBatchSafe(dStmt, "DEL", dNums.toSeq)
        conn.commit()
        onCancel(lastRowNum)
        reportAp(ic, uc, dc, sc, si, su, sd, true)
      } else {
        if (iNums.nonEmpty) executeBatchSafe(iStmt, "INS", iNums.toSeq)
        if (uNums.nonEmpty) executeBatchSafe(uStmt, "UPD", uNums.toSeq)
        if (dNums.nonEmpty) executeBatchSafe(dStmt, "DEL", dNums.toSeq)
        conn.commit()
        reportAp(ic, uc, dc, sc, si, su, sd, true)
      }

    } catch {
      case e: Exception =>
        conn.rollback()
        val msg ="[Apply Target Stop]".red + s" ${plan.name} last transaction rolled back. Reason: ${e.getMessage}"
        noticeWithLog(notice){
          ReportMsgAbort(plan.name, msg)
        }
    } finally {
      iStmt.close()
      uStmt.close()
      dStmt.close()
      closeOutStream()
    }
  }

}


object DiffApplier {

  def bindRow(stmt: PreparedStatement,
              values: List[CVal],
              indices: List[(String, Int, String)],
              debug: Boolean): Unit = {

    var paramIdx = 1
    val it = values.iterator
    val it0 = indices.iterator

    while (it.hasNext && it0.hasNext) {
      val cval = it.next()
      val (cname, ctype, tname) = it0.next()

      def log(action: => String): Unit = {
        if (debug) memo(s"[BIND] idx=$paramIdx (Double Bind) ($cname:$ctype:$tname) -> $action")
      }

      // 모든 플레이스홀더 패턴이 컬럼당 정확히 2개씩 매립되어 있으므로 연속으로 2번 할당합니다.
      def doubleBind(binder: Int => Unit): Unit = {
        binder(paramIdx)
        binder(paramIdx + 1)
      }

      cval match {
        case a: CVOrderable => a match {
          case CInt(_, value) => log(s"setInt($value)")
            doubleBind(idx => stmt.setInt(idx, value))

          case CLong(_, value) => log(s"setLong($value)")
            doubleBind(idx => stmt.setLong(idx, value))

          case CBigInt(_, value) => log(s"setBigDecimal(${value.bigInteger})")
            doubleBind(idx => stmt.setBigDecimal(idx, new java.math.BigDecimal(value.bigInteger)))

          case CDouble(_, value) => log(s"setDouble($value)")
            doubleBind(idx => stmt.setDouble(idx, value))

          case CDecimal(_, value) => log(s"setBigDecimal($value)")
            doubleBind(idx => stmt.setBigDecimal(idx, value.bigDecimal))

          case CString(_, value) => log(s"setString($value)")
            doubleBind(idx => stmt.setString(idx, value))

          case CDate(_, value) => log(s"setDate($value)")
            doubleBind(idx => stmt.setDate(idx, java.sql.Date.valueOf(value)))

          case CTime(_, value) => log(s"setTime($value)")
            doubleBind(idx => stmt.setTime(idx, java.sql.Time.valueOf(value)))

          case CTimestamp(_, value) => log(s"setTimestamp($value)")
            doubleBind(idx => stmt.setTimestamp(idx, java.sql.Timestamp.valueOf(value)))

          case COffsetTime(_, value) => log(s"setObject(OffsetTime=$value)")
            doubleBind(idx => stmt.setObject(idx, value))

          case COffsetTimestamp(_, value) => log(s"setObject(OffsetTimestamp=$value)")
            doubleBind(idx => stmt.setObject(idx, value))
        }

        case a: CVEquatable => a match {
          case CBoolean(_, value) => log(s"setBoolean($value)")
            doubleBind(idx => stmt.setBoolean(idx, value))

          case CBytes(_, value) => log(s"setBytes(${value.length})")
            doubleBind(idx => stmt.setBytes(idx, value))

          case x: CLongString =>
            val str = x.longString.makeString
            doubleBind { idx =>
              if (ctype == Types.LONGVARCHAR || ctype == Types.LONGNVARCHAR) {
                val reader = new java.io.StringReader(str)
                log(s"setCharacterStream(len=${str.length})")
                stmt.setCharacterStream(idx, reader, str.length)
              } else {
                val reader = new StringReader(str)
                log(s"setClob(len=${str.length})")
                stmt.setClob(idx, reader, str.length)
              }
            }

          case x: CLongBytes =>
            val bytes = x.longBytes.makeBytes
            doubleBind { idx =>
              val is = new java.io.ByteArrayInputStream(bytes)
              log(s"setBinaryStream(len=${bytes.length})")
              stmt.setBinaryStream(idx, is, bytes.length)
            }

          case x: COraGeometry =>
            val conn = stmt.getConnection
            val ora = conn.unwrap(classOf[OracleConnection])
            val struct = if (x.value != null) {
              log("toStruct + setObject(STRUCT)")
              oracle.spatial.geometry.JGeometry.storeJS(x.value, ora)
            } else {
              log("null geometry → setObject(null, STRUCT)")
              null
            }
            doubleBind(idx => stmt.setObject(idx, struct, java.sql.Types.STRUCT))

          case CInterval(_, value) => log(s"setString(INTERVAL=$value)")
            doubleBind(idx => stmt.setString(idx, value))

          case CXML(_, value) => log(s"setString(XMLString=$value)")
            doubleBind(idx => stmt.setString(idx, value))
        }

        case a: CVIncomparable => a match {
          case CRowID(_, value) => log(s"setString(ROWID=$value)")
            doubleBind(idx => stmt.setString(idx, value))

          case CBFile(_, value) => log(s"setString(BFILE=$value)")
            doubleBind(idx => stmt.setString(idx, value))

          case CNull(_) =>
            doubleBind { idx =>
              ctype match {
                case Types.OTHER =>
                  if (tname == "MDSYS.SDO_GEOMETRY") {
                    log("setNull(OTHER, MDSYS.SDO_GEOMETRY)")
                    stmt.setNull(idx, Types.STRUCT, "MDSYS.SDO_GEOMETRY")
                  } else {
                    log("setObject(null) [OTHER]")
                    stmt.setObject(idx, null)
                  }
                case Types.STRUCT =>
                  if (tname == "MDSYS.SDO_GEOMETRY") {
                    log("setNull(STRUCT: MDSYS.SDO_GEOMETRY)")
                    stmt.setNull(idx, Types.STRUCT, "MDSYS.SDO_GEOMETRY")
                  } else {
                    log(s"setNull(STRUCT: ${tname})")
                    stmt.setNull(idx, Types.STRUCT)
                  }

                case _ => log(s"setNull($ctype: ${tname} )")
                  stmt.setNull(idx, ctype)
              }
            }
          case CNotSupport(_, jdbcType) =>
            throw new IllegalArgumentException(s"[bindRow: not support] idx=$paramIdx col=$cname jdbcType=$jdbcType")
        }
      }
      paramIdx += 2
    }
  }

  def sqlForApply(plan: ComparePlan) = {

    def typeName(name: String): String =
      Option(name).map(_.toUpperCase match {
        case n if n.endsWith("SDO_GEOMETRY") => "MDSYS.SDO_GEOMETRY"
        case "LONG" => "LONG"
        case "LONG RAW" => "LONG RAW"
        case other => other
      }).orNull

    def typeCode(jdbcType: Int, name: String): Int = {
      val upper = Option(typeName(name)).map(_.toUpperCase)
      upper match {
        case Some(n) if n.endsWith("SDO_GEOMETRY") => Types.STRUCT
        case Some("LONG") => Types.LONGVARCHAR
        case Some("LONG RAW") => Types.LONGVARBINARY
        case _ => jdbcType
      }
    }

    val tableName = plan.table.name
    val fullTableName = plan.table.schema.map(_ + ".").getOrElse("") + tableName

    val keyNames = plan.compRow.sortKey.map(_.name)
    val keyIndices = keyNames.map { name =>
      plan.sourceReaders.find(_.name == name).map((r: CReader) =>
        (r.name, typeCode(r.jdbcType, r.typeName), typeName(r.typeName))).getOrElse(
        throw new IllegalStateException(
          s"[Meta Mapping Error] PK Column '$name' defined in ComparePlan was not found in the source SELECT query (CReader list)."
        )
      )
    }
    val valNames = plan.compRow.compCols.filterNot(_.isVirtual).map(_.name) ++ plan.compRow.compLobs.map(_.name)
    val valIndices = valNames.map { name =>
      plan.sourceReaders.find(_.name == name).map((r: CReader) => (r.name, typeCode(r.jdbcType, r.typeName), typeName(r.typeName))).getOrElse(
        throw new IllegalStateException(
          s"[Meta Mapping Error] Physical Column '$name' defined in ComparePlan was not found in sourceReaders."
        )
      )
    }

    // INSERT VALUES 절 규격 맞춤 (COALESCE 함수를 통과시켜 각 컬럼당 물음표 2개씩 소모)
    def toValuesPlaceholders(colNames: Seq[String]): String = {
      colNames.map(_ => "COALESCE(?, ?)" ).mkString(", ")
    }

    // UPDATE SET 절 규격 맞춤 (각 컬럼당 물음표 2개씩 소모)
    def toUpdatePlaceholders(colNames: Seq[String]): String = {
      colNames.map(name => s"$name = COALESCE(?, ?)").mkString(", ")
    }

    // WHERE 절 만능 표준 템플릿 적용 (각 컬럼당 물음표 2개씩 소모)
    def toWherePlaceholders(colNames: Seq[String]): String = {
      colNames.map(name => s"($name = ? OR ($name IS NULL AND ? IS NULL))").mkString(" AND ")
    }

    val insertSql = s"INSERT INTO $fullTableName (${(keyNames ++ valNames).mkString(", ")}) VALUES (${toValuesPlaceholders(keyNames ++ valNames)})"
    val updateSql = s"UPDATE $fullTableName SET ${toUpdatePlaceholders(valNames)} WHERE ${toWherePlaceholders(keyNames)}"
    val deleteSql = s"DELETE FROM $fullTableName WHERE ${toWherePlaceholders(keyNames)}"

    (insertSql, updateSql, deleteSql, keyIndices, valIndices)
  }

  // --------------------------------------------------------------------------------

  /**
   * 0. diffNums는 오름차순 정렬되어 있고, lastDiffNum보다 작거나 같다.
   * 1. diffNums에 있는 번호의 값을 모두 diffs에서 꺼내야 한다.
   * 2. lastDiffNum보다 큰 번호의 값도 모두 diffs에서 꺼내야 한다.
   */
  def extractDiffs(diffs: Iterator[(Int, DiffRow)],
                   diffNums: InputStream,
                   lastDiffNum: Option[Int] = None ): Iterator[(Int, DiffRow)] = {

    val reader = new BufferedReader(new InputStreamReader(diffNums, "UTF-8"))

    new Iterator[(Int, DiffRow)] {

      private var currentDiff: (Int, DiffRow) = null
      private var hasDiff: Boolean = advanceDiff()

      private var currentFailNum: Int = -1
      private var hasFail: Boolean = advanceFail()

      private var readyElement: (Int, DiffRow) = null
      private var isReady: Boolean = false
      private var closed: Boolean = false

      private def advanceDiff(): Boolean = {
        if (diffs.hasNext) {
          currentDiff = diffs.next()
          true
        } else {
          currentDiff = null
          false
        }
      }

      private def advanceFail(): Boolean = {
        var line = reader.readLine()
        while (line != null) {
          val trimmed = line.trim
          if (trimmed.nonEmpty) {
            currentFailNum = trimmed.toInt
            return true
          }
          line = reader.readLine()
        }
        hasFail = false
        false
      }

      private def closeReader(): Unit = {
        if (!closed) {
          try reader.close() catch {
            case _: Exception =>
          }
          closed = true
        }
      }

      override def hasNext: Boolean = {
        if (isReady) return true

        while (hasDiff) {
          val diffNum = currentDiff._1

          if(lastDiffNum.exists( diffNum > _)) {
            readyElement = currentDiff
            isReady = true
            hasDiff = advanceDiff()
            return true
          }

          if (hasFail) {
            val failNum = currentFailNum
            if (diffNum < failNum) {
              hasDiff = advanceDiff()
            } else if (diffNum > failNum) {
              hasFail = advanceFail()
            } else {
              readyElement = currentDiff
              isReady = true
              hasDiff = advanceDiff()
              hasFail = advanceFail()
              return true
            }
          } else {
            hasDiff = advanceDiff()
          }
        }

        reader.close()
        false
      }

      override def next(): (Int, DiffRow) = {
        if (!hasNext) {
          reader.close()
          throw new NoSuchElementException
        }
        val result = readyElement
        readyElement = null
        isReady = false
        result
      }
    }
  }

  // --------------------------------------------------------------------------------

}
