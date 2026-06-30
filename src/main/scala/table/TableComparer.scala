package table

import oracle.jdbc.OracleConnection
import schema.ComparePlan
import tui.layoutzEx._
import utils.LogHelper.{memo, note, oops}

import java.io.StringReader
import java.sql.{Connection, PreparedStatement, ResultSet, Types}
import java.time.{Instant, ZoneId}
import javax.sql.DataSource

class TableComparer(plan: ComparePlan, isDebug: Boolean = false) {

  def compareIt(srcDs: DataSource, tgtDs: DataSource,
                updateState: (Long, Long, Long, Long, Long, Long, String, Boolean) => Unit,
                reportInterval: Int = 521
               ): Iterator[DiffRow] with AutoCloseable = {

    val srcConn = srcDs.getConnection
    val tgtConn = tgtDs.getConnection

    val srcStmt = srcConn.prepareStatement(plan.sourceSql)
    val tgtStmt = tgtConn.prepareStatement(plan.targetSql)

    srcStmt.setFetchSize(256)
    tgtStmt.setFetchSize(256)

    val startMillis: Long = System.currentTimeMillis()
    val startTime = Instant.ofEpochMilli(startMillis).atZone(ZoneId.systemDefault()).toLocalDateTime()

    val startStr = s"start $startTime"
    updateState(0,0,0,0,0,0,startStr, false)
    val srcRs = srcStmt.executeQuery()
    val tgtRs = tgtStmt.executeQuery()

    def elapseLog(start: Long = startMillis) = {
      val now: Long = System.currentTimeMillis()
      val elapse = (now - start)/ 1000.0
      updateState(0,0,0,0,0,0,"["+ f"$elapse%.3f".color(Color.Red).render + s" sec] since $startTime", false)
    }
    elapseLog()

    def debugLog(s: => String) = if(isDebug) memo(s)

    new Iterator[DiffRow] with AutoCloseable {

      private var srcHasNext = srcRs.next()
      private var tgtHasNext = tgtRs.next()
      private var nextBuffer: Option[DiffRow] = None

      // ------
      private var readCount1 = 0L
      private var readCount2 = 0L
      private var sameCount  = 0L
      private var updCount   = 0L
      private var onlyACount = 0L
      private var onlyBCount = 0L

      // ------
      private var curSrcRow: IndexedSeq[CVal] = if (srcHasNext) readRowSequentially(srcRs, sortedSrcReaders, "SOURCE") else null
      private var curTgtRow: IndexedSeq[CVal] = if (tgtHasNext) readRowSequentially(tgtRs, sortedTgtReaders, "TARGET") else null

      // ------
      @inline private def notice(msg: String = "", fin: Boolean = false): Unit = {
        val sum = readCount1 + readCount2
        if (fin || (sum % reportInterval == 0)) {
          val millis = System.currentTimeMillis()
          val now = Instant.ofEpochMilli(millis).atZone(ZoneId.systemDefault()).toLocalDateTime
          val str = startStr + "--" + now.toString + msg
          updateState(readCount1, readCount2, sameCount, updCount, onlyACount, onlyBCount, str, fin)
        }
      }

      private def readRowSequentially(rs: ResultSet, readers: List[CReader], label: String): IndexedSeq[CVal] = {
        debugLog(s"\n============ [$label] Row Extraction Start ============")
        val extracted = readers.map { r =>
          try {
            val cval = r.read(rs)
            debugLog(f"  [Idx ${r.index}%2d] ${r.name}%-16s (${r.typeName}%-10s) => $cval")
            cval
          } catch {
            case e: Exception =>
              println(s"  [READ ERROR at Idx ${r.index}] Column: ${r.name}, Type: ${r.typeName} JDBCType: ${r.jdbcType}")
              println(s"  Reason: ${e.getMessage}")
              throw e
          }
        }.toIndexedSeq
        debugLog(s"============ [$label] Row Extraction End ==============\n")
        if((readCount1 + readCount2) == 0L) elapseLog()

        extracted
      }

      private def advanceSource(): Unit = {
        srcHasNext = srcRs.next()
        readCount1 += 1
        curSrcRow = if (srcHasNext) readRowSequentially(srcRs, sortedSrcReaders, "SOURCE_DB") else null
      }

      private def advanceTarget(): Unit = {
        tgtHasNext = tgtRs.next()
        readCount2 += 1
        curTgtRow = if (tgtHasNext) readRowSequentially(tgtRs, sortedTgtReaders, "TARGET_DB") else null
      }

      override def hasNext: Boolean = {
        try{
          hasNext0
        } catch {case e: Throwable =>
          println(s"--- compareIt : ${e.getMessage}")
          e.printStackTrace()
          throw e
        }
      }

      def hasNext0: Boolean = {
        if (nextBuffer.isDefined) return true

        while (srcHasNext && tgtHasNext && nextBuffer.isEmpty) {
          val keyDiff = compareKeys(curSrcRow, curTgtRow, isDebug)

          if (keyDiff == 0) {
            if (!equalValues(curSrcRow, curTgtRow, isDebug)) {
              updCount += 1
              nextBuffer = Some(Update( extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            } else {
              sameCount += 1
              nextBuffer = Some(Same(extractSourceKeys(curSrcRow)))
            }
            notice()
            advanceSource()
            advanceTarget()
          } else if (keyDiff < 0) {
            onlyACount += 1
            nextBuffer = Some(OnlyInA(extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            notice()
            advanceSource()
          } else {
            onlyBCount += 1
            nextBuffer = Some(OnlyInB(extractTargetKeys(curTgtRow)))
            notice()
            advanceTarget()
          }
        }

        if (nextBuffer.isEmpty) {
          if (srcHasNext) {
            onlyACount += 1
            nextBuffer = Some(OnlyInA(extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            notice()
            advanceSource()
          } else if (tgtHasNext) {
            onlyBCount += 1
            nextBuffer = Some(OnlyInB(extractTargetKeys(curTgtRow)))
            notice()
            advanceTarget()
          }
        }

        if (nextBuffer.isEmpty) close()
        nextBuffer.isDefined
      }

      override def next(): DiffRow = {
        if (!hasNext) {
          notice(msg = "No more diff data.")
          throw new NoSuchElementException("No more diff data.")
        }
        val res = nextBuffer.get
        nextBuffer = None
        res
      }

      override def close(): Unit = {

        notice(fin= true)

        Option(srcRs).foreach(_.close())
        Option(tgtRs).foreach(_.close())
        Option(srcStmt).foreach(_.close())
        Option(tgtStmt).foreach(_.close())
        Option(srcConn).foreach(_.close())
        Option(tgtConn).foreach(_.close())
      }
    }
  }

  // --------------------------------------------------------------------------------
  private val keyComps: List[(CValComp, Int, Int)] = plan.keyComps
  private val colComps: List[(CValComp, Int, Int)] = plan.colComps
  private val lobComps: List[(CValComp, Int, Int)] = plan.lobComps

  private val sortedSrcReaders = plan.sourceReaders.sortBy(_.index)
  private val sortedTgtReaders = plan.targetReaders.sortBy(_.index)

  private def compareKeys(srcRow: IndexedSeq[CVal], tgtRow: IndexedSeq[CVal], debug: Boolean): Int = {
    var diff = 0
    val it = keyComps.iterator
    while (diff == 0 && it.hasNext) {
      val (comp, colA, colB) = it.next()
      diff = comp.compare(srcRow(colA - 1), tgtRow(colB - 1))
      if (debug && (diff != 0))
        note("diff: " + srcRow(colA - 1) + " ---" + tgtRow(colB - 1))
    }
    diff
  }

  private def equalValues(srcRow: IndexedSeq[CVal], tgtRow: IndexedSeq[CVal], debug: Boolean): Boolean = {
    val colIt = colComps.iterator
    while (colIt.hasNext) {
      val (comp, colA, colB) = colIt.next()
      val eq = comp.equal(srcRow(colA - 1), tgtRow(colB - 1))
      if (debug && !eq) {
        note("diff: " + srcRow(colA - 1) + " ---" + tgtRow(colB - 1))
        return false
      }
    }

    val lobIt = lobComps.iterator
    while (lobIt.hasNext) {
      val (comp, colA, colB) = lobIt.next()
      val eq = comp.equal(srcRow(colA - 1), tgtRow(colB - 1))
      if (debug && !eq) {
        note("diff: " + srcRow(colA - 1) + " ---" + tgtRow(colB - 1))
        return false
      }
    }
    true
  }

  private def extractSourceKeys(row: IndexedSeq[CVal]): List[CVal] = keyComps.map { case (_, colA, _) => row(colA - 1) }
  private def extractTargetKeys(row: IndexedSeq[CVal]): List[CVal] = keyComps.map { case (_, _, colB) => row(colB - 1) }
  private def extractSourceVals(row: IndexedSeq[CVal]): List[CVal]
  = colComps.map { case (_, colA, _) => row(colA - 1) } ++ lobComps.map { case (_, colA, _) => row(colA - 1) }

}

object TableComparer {

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
        case Some("LONG")     => Types.LONGVARCHAR
        case Some("LONG RAW") => Types.LONGVARBINARY
        case _                => jdbcType
      }
    }

    val tableName = plan.table.name
    val fullTableName = plan.table.schema.map(_ + ".").getOrElse("") + tableName

    val keyNames = plan.compRow.sortKey.map(_.name)
    val keyIndices= keyNames.map { name =>
      plan.sourceReaders.find(_.name == name).map(r => (r.name, typeCode(r.jdbcType, r.typeName), typeName(r.typeName))).getOrElse(
        throw new IllegalStateException(
          s"[Meta Mapping Error] PK Column '$name' defined in ComparePlan was not found in the source SELECT query (CReader list). " +
            s"Please check if the SELECT query contains this PK column."
        )
      )
    }
    val valNames = plan.compRow.compCols.filterNot(_.isVirtual).map(_.name) ++ plan.compRow.compLobs.map(_.name)
    val valIndices= valNames.map { name =>
      plan.sourceReaders.find(_.name == name).map(r => (r.name, typeCode(r.jdbcType, r.typeName), typeName(r.typeName))).getOrElse(
        throw new IllegalStateException(
          s"[Meta Mapping Error] Physical Column '$name' defined in ComparePlan was not found in sourceReaders. " +
            s"Please check if the column name is misspelled or missing in sourceReaders."
        )
      )
    }


    def toValuesPlaceholders(colNames: Seq[String]): String = { colNames.map(_ => "?").mkString(", ") }
    def toUpdatePlaceholders(colNames: Seq[String]): String = { colNames.map(name => s"$name = ?").mkString(", ") }
    def toWherePlaceholders(colNames: Seq[String]): String = { colNames.map(name => s"$name = ?").mkString(" AND ") }

    val insertSql = s"INSERT INTO $fullTableName (${(keyNames ++ valNames).mkString(", ")}) VALUES (${toValuesPlaceholders(keyNames ++ valNames)})"
    val updateSql = s"UPDATE $fullTableName SET ${toUpdatePlaceholders(valNames)} WHERE ${toWherePlaceholders(keyNames)}"
    val deleteSql = s"DELETE FROM $fullTableName WHERE ${toWherePlaceholders(keyNames)}"

    (insertSql, updateSql, deleteSql, keyIndices, valIndices)
  }

  /** ------------------------------ */
  def applyChanges(plan: ComparePlan, diffs: Iterator[DiffRow], targetConn: Connection,
                   notice: (Long, Long, Long, Long, Boolean) => Unit,
                   debug: Boolean,
                   batchSize: Int = 512) = {

    val insStmt = targetConn.prepareStatement(plan.insertSql)
    val updStmt = targetConn.prepareStatement(plan.updateSql)
    val delStmt = targetConn.prepareStatement(plan.deleteSql)

    targetConn.setAutoCommit(false)
    var insCount, updCount, delCount, skiCount = 0L

    try {
      val valIndices= plan.valIndices
      val keyIndices= plan.keyIndices
      val hasLongColumn = valIndices.exists(i => i._2 == java.sql.Types.LONGVARCHAR || i._2 == java.sql.Types.LONGNVARCHAR)

      diffs.foreach {
        // insert -----------
        case OnlyInA(keys, sourceVals) =>
          val ai = keyIndices ++ valIndices
          bindRow(insStmt, keys ++ sourceVals, ai, debug)
          if(hasLongColumn) {
            insStmt.executeUpdate()
            insCount += 1
            if (insCount % batchSize == 0) notice(insCount, updCount, delCount, skiCount, false)
          } else {
            insStmt.addBatch()
            insCount += 1
            if (insCount % batchSize == 0) {
              insStmt.executeBatch()
              notice(insCount, updCount, delCount, skiCount, false)
            }
          }

        // update -----------
        case Update(keys, sourceVals) =>
          val ui = valIndices ++ keyIndices
          bindRow(updStmt, sourceVals ++ keys, ui, debug)
          if(hasLongColumn) {
            updStmt.executeUpdate()
            updCount += 1
            if (updCount % batchSize == 0) notice(insCount, updCount, delCount, skiCount, false)
          }
          else {
            updStmt.addBatch()
            updCount += 1
            if (updCount % batchSize == 0) {
              updStmt.executeBatch()
              notice(insCount, updCount, delCount, skiCount, false)
            }
          }

        // delete -----------
        case OnlyInB(keys) =>
          val di = keyIndices
          bindRow(delStmt, keys, di, debug)
          delStmt.addBatch()
          delCount += 1
          if (delCount % batchSize == 0) {
            delStmt.executeBatch()
            notice(insCount, updCount, delCount, skiCount, false)
          }

        case Same(_) =>
          skiCount += 1
          if (skiCount % batchSize == 0) {
            notice(insCount, updCount, delCount, skiCount, false)
          }
      }

      // remains ------------------------
      if(!hasLongColumn){
        if (insCount % batchSize != 0) insStmt.executeBatch()
        if (updCount % batchSize != 0) updStmt.executeBatch()
      }
      if (delCount % batchSize != 0) delStmt.executeBatch()

      targetConn.commit()
      notice(insCount, updCount, delCount, skiCount, true)

    } catch {
      case e: Exception =>
        notice(insCount, updCount, delCount, skiCount, true)
        targetConn.rollback()
        // todo :: g3nie
        oops("[Apply Target Stop]".color(Color.Red).render + s" ${plan.name} last transaction rolled back. Reason: ${e.getMessage}")
    } finally {
      insStmt.close()
      updStmt.close()
      delStmt.close()
    }
  }

  private def bindRow( stmt: PreparedStatement,
                       values: List[CVal],
                       indices: List[(String, Int, String)],
                       debug: Boolean): Unit = {

    var paramIdx = 1
    val it = values.iterator
    val it0 = indices.iterator

    while (it.hasNext && it0.hasNext) {
      val cval = it.next()
      val (cname, ctype, tname) = it0.next()

      def log(action: => String): Unit
      = { if(debug) memo(s"[BIND] idx=$paramIdx ($cname:$ctype:$tname) -> $action") }

      cval match {

        case a: CVOrderable => a match {
          case CInt(_, value) => log(s"setInt($value)")
            stmt.setInt(paramIdx, value)

          case CLong(_, value) => log(s"setLong($value)")
            stmt.setLong(paramIdx, value)

          case CBigInt(_, value) => log(s"setBigDecimal(${value.bigInteger})")
            stmt.setBigDecimal(paramIdx, new java.math.BigDecimal(value.bigInteger))

          case CDouble(_, value) => log(s"setDouble($value)")
            stmt.setDouble(paramIdx, value)

          case CDecimal(_, value) => log(s"setBigDecimal($value)")
            stmt.setBigDecimal(paramIdx, value.bigDecimal)

          case CString(_, value) => log(s"setString($value)")
            stmt.setString(paramIdx, value)

          case CDate(_, value) => log(s"setDate($value)")
            stmt.setDate(paramIdx, java.sql.Date.valueOf(value))

          case CTime(_, value) => log(s"setTime($value)")
            stmt.setTime(paramIdx, java.sql.Time.valueOf(value))

          case CTimestamp(_, value) => log(s"setTimestamp($value)")
            stmt.setTimestamp(paramIdx, java.sql.Timestamp.valueOf(value))

          case COffsetTime(_, value) => log(s"setObject(OffsetTime=$value)")
            stmt.setObject(paramIdx, value)

          case COffsetTimestamp(_, value) => log(s"setObject(OffsetTimestamp=$value)")
            stmt.setObject(paramIdx, value)
        }

        case a: CVEquatable => a match {

          case CBoolean(_, value) => log(s"setBoolean($value)")
            stmt.setBoolean(paramIdx, value)

          case CBytes(_, value) => log(s"setBytes(${value.length})")
            stmt.setBytes(paramIdx, value)

          case x: CLongString =>
            val str = x.longString.makeString
            if(ctype == Types.LONGVARCHAR || ctype == Types.LONGNVARCHAR ) {
              val reader = new java.io.StringReader(str)
              log(s"setCharacterStream(len=${str.length})")
              stmt.setCharacterStream(paramIdx, reader, str.length)
            } else {
              val reader = new StringReader(str)
              log(s"setClob(len=${str.length})")
              stmt.setClob(paramIdx, reader, str.length)
            }

          case x: CLongBytes =>
            val bytes = x.longBytes.makeBytes
            val is = new java.io.ByteArrayInputStream(bytes)
            log(s"setBinaryStream(len=${bytes.length})")
            stmt.setBinaryStream(paramIdx, is, bytes.length)

          case x: COraGeometry =>
            val conn = stmt.getConnection
            val ora = conn.unwrap(classOf[OracleConnection])
            val struct =
              if (x.value != null) {
                log("toStruct + setObject(STRUCT)")
                oracle.spatial.geometry.JGeometry.storeJS(x.value, ora)
              } else {
                log("null geometry → setObject(null, STRUCT)")
                null
              }
            stmt.setObject(paramIdx, struct, java.sql.Types.STRUCT)

          case CInterval(_, value) =>
            log(s"setString(INTERVAL=$value)")
            stmt.setString(paramIdx, value)

          case CXML(_, value) =>
            log(s"setString(XMLString=$value)")
            stmt.setString(paramIdx, value)
        }

        case a: CVIncomparable => a match {

          case CRowID(_, value) => log(s"setString(ROWID=$value)")
            stmt.setString(paramIdx, value)

          case CBFile(_, value) => log(s"setString(BFILE=$value)")
            stmt.setString(paramIdx, value)

          case CNull(_) =>
            ctype match {
              case Types.OTHER =>
                if(tname == "MDSYS.SDO_GEOMETRY") { // idiot oracle
                  log("setNull(STRUCT, MDSYS.SDO_GEOMETRY)")
                  stmt.setNull(paramIdx, Types.STRUCT, "MDSYS.SDO_GEOMETRY")
                } else{
                  log("setObject(null) [OTHER]")
                  stmt.setObject(paramIdx, null)
                }

              case Types.STRUCT => log("setNull(STRUCT)")
                stmt.setNull(paramIdx, Types.STRUCT)

              case _ => log(s"setNull($ctype)")
                stmt.setNull(paramIdx, ctype)
            }

          case CNotSupport(_, jdbcType) =>
            throw new IllegalArgumentException(s"[bindRow: not support] idx=$paramIdx col=$cname jdbcType=$jdbcType")
        }
      }
      paramIdx += 1
    }
  }

}
