package table

import schema.ComparePlan
import utils.LogHelper.memo

import java.sql.{Connection, PreparedStatement, ResultSet, Types}
import java.time.{Instant, ZoneId}
import javax.sql.DataSource

class TableComparer(plan: ComparePlan, isDebug: Boolean = false) {

  def compareIt(srcDs: DataSource, tgtDs: DataSource,
                updateState: (Long, Long, Long, Long, Long, Long, String) => Unit
               = {case (r1, r2, s, u, a, b, m) => println(s"$m r1:$r1 r2:$r2 same:$s update:$u onlyA:$a onlyB:$b") },
                reportInterval: Int = 64
               ): Iterator[DiffRow] with AutoCloseable = {

    val srcConn = srcDs.getConnection
    val tgtConn = tgtDs.getConnection

    val srcStmt = srcConn.prepareStatement(plan.sourceSql)
    val tgtStmt = tgtConn.prepareStatement(plan.targetSql)

    srcStmt.setFetchSize(256)
    tgtStmt.setFetchSize(256)

    val startMillis: Long = System.currentTimeMillis()
    val startTime = Instant.ofEpochMilli(startMillis).atZone(ZoneId.systemDefault()).toLocalDateTime()

    updateState(0,0,0,0,0,0,s"start : $startTime")
    val srcRs = srcStmt.executeQuery()
    val tgtRs = tgtStmt.executeQuery()

    val firstMillis: Long = System.currentTimeMillis()
    val elapse = (firstMillis - startMillis)/ 1000.0
    updateState(0,0,0,0,0,0,f"first : $elapse%.3f sec")

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
      @inline private def triggerStateUpdate(stateMessage: String = ""): Unit = {
        val totalProcessed = sameCount + updCount + onlyACount + onlyBCount
//        if (totalProcessed % reportInterval == 1) {
//          updateState(readCount1, readCount2, sameCount, updCount, onlyACount, onlyBCount, stateMessage)
//        }
      }

      private def readRowSequentially(rs: ResultSet, readers: List[CReader], label: String): IndexedSeq[CVal] = {
        if (isDebug) {
          println(s"\n============ [$label] Row Extraction Start ============")
        }

        val extracted = readers.map { r =>
          try {
            val cval = r.read(rs)
            if (isDebug) {
              println(f"  [Idx ${r.index}%2d] ${r.name}%-16s (${r.typeName}%-10s) => $cval")
            }
            cval
          } catch {
            case e: Exception =>
              println(s"  [READ ERROR at Idx ${r.index}] Column: ${r.name}, Type: ${r.typeName}")
              println(s"  Reason: ${e.getMessage}")
              throw e
          }
        }.toIndexedSeq
        if (isDebug) {
          println(s"============ [$label] Row Extraction End ==============\n")
        }
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
        if (nextBuffer.isDefined) return true

        while (srcHasNext && tgtHasNext && nextBuffer.isEmpty) {
          val keyDiff = compareKeys(curSrcRow, curTgtRow)

          if (keyDiff == 0) {
            if (!equalValues(curSrcRow, curTgtRow)) {
              updCount += 1
              nextBuffer = Some(Update( extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            } else {
              sameCount += 1
              nextBuffer = Some(Same(extractSourceKeys(curSrcRow)))
            }
            triggerStateUpdate("compare")
            advanceSource()
            advanceTarget()
          } else if (keyDiff < 0) {
            onlyACount += 1
            nextBuffer = Some(OnlyInA(extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            triggerStateUpdate("compare")
            advanceSource()
          } else {
            onlyBCount += 1
            nextBuffer = Some(OnlyInB(extractTargetKeys(curTgtRow)))
            triggerStateUpdate("compare")
            advanceTarget()
          }
        }

        if (nextBuffer.isEmpty) {
          if (srcHasNext) {
            onlyACount += 1
            nextBuffer = Some(OnlyInA(extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            triggerStateUpdate("remain")
            advanceSource()
          } else if (tgtHasNext) {
            onlyBCount += 1
            nextBuffer = Some(OnlyInB(extractTargetKeys(curTgtRow)))
            triggerStateUpdate("remain")
            advanceTarget()
          }
        }

        if (nextBuffer.isEmpty) close()
        nextBuffer.isDefined
      }

      override def next(): DiffRow = {
        if (!hasNext) throw new NoSuchElementException("No more diff data.")
        val res = nextBuffer.get
        nextBuffer = None
        res
      }

      override def close(): Unit = {

        triggerStateUpdate("fin")

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

  private def compareKeys(srcRow: IndexedSeq[CVal], tgtRow: IndexedSeq[CVal]): Int = {
    var diff = 0
    val it = keyComps.iterator
    while (diff == 0 && it.hasNext) {
      val (comp, colA, colB) = it.next()
      diff = comp.compare(srcRow(colA - 1), tgtRow(colB - 1))
      if (diff != 0) println("diff: " + srcRow(colA - 1) + " ---" + tgtRow(colB - 1))
    }
    diff
  }

  private def equalValues(srcRow: IndexedSeq[CVal], tgtRow: IndexedSeq[CVal]): Boolean = {
    val colIt = colComps.iterator
    while (colIt.hasNext) {
      val (comp, colA, colB) = colIt.next()
      if (!comp.equal(srcRow(colA - 1), tgtRow(colB - 1))) {
        memo("diff(n): " + srcRow(colA - 1) + " ---" + tgtRow(colB - 1))
        return false
      }
    }

    val lobIt = lobComps.iterator
    while (lobIt.hasNext) {
      val (comp, colA, colB) = lobIt.next()
      if (!comp.equal(srcRow(colA - 1), tgtRow(colB - 1))) {
        memo("diff(l):" + srcRow(colA - 1) + " ---" + tgtRow(colB - 1))
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

    val tableName = plan.table.name
    val fullTableName = plan.table.schema.map(_ + ".").getOrElse("") + tableName

    val keyNames = plan.compRow.sortKey.map(_.name)
    val keyIndices= keyNames.map { name =>
      val foundIdx = plan.sourceReaders.indexWhere(_.name == name)
      if (foundIdx == -1) {
        throw new IllegalStateException(
          s"[Meta Mapping Error] PK Column '$name' defined in ComparePlan was not found in the source SELECT query (CReader list). " +
            s"Please check if the SELECT query contains this PK column."
        )
      }
      foundIdx
    }
    val valNames = plan.compRow.compCols.filterNot(_.isVirtual).map(_.name) ++ plan.compRow.compLobs.map(_.name)
    val valIndices= valNames.map { name =>
      val foundIdx = plan.sourceReaders.indexWhere(_.name == name)
      if (foundIdx == -1) {
        throw new IllegalStateException(
          s"[Meta Mapping Error] Physical Column '$name' defined in ComparePlan was not found in sourceReaders. " +
            s"Please check if the column name is misspelled or missing in sourceReaders."
        )
      }
      foundIdx
    }

    def toValuesPlaceholders(colNames: Seq[String]): String = { colNames.map(_ => "?").mkString(", ") }
    def toUpdatePlaceholders(colNames: Seq[String]): String = { colNames.map(name => s"$name = ?").mkString(", ") }
    def toWherePlaceholders(colNames: Seq[String]): String = { colNames.map(name => s"$name = ?").mkString(" AND ") }

    val insertSql = s"INSERT INTO $fullTableName (${(keyNames ++ valNames).mkString(", ")}) VALUES (${toValuesPlaceholders(keyNames ++ valNames)})"
    val updateSql = s"UPDATE $fullTableName SET ${toUpdatePlaceholders(valNames)} WHERE ${toWherePlaceholders(keyNames)}"
    val deleteSql = s"DELETE FROM $fullTableName WHERE ${toWherePlaceholders(keyNames)}"

    (insertSql, updateSql, deleteSql, keyIndices, valIndices)
  }


  import tui.layoutzEx._
  def showUpdateState(ic: Long, uc: Long, dc: Long, sc: Long, fin: Boolean)
  = println(s"insert $ic update $uc delete $dc skip $sc fin $fin".color(Color.Yellow).render)

  /**
   * 파일이나 스트림에서 읽어온 DiffRow 데이터를 Target DB에 Batch로 밀어 넣는 핵심 메서드
   *
   * @param targetConn Target DB 커넥션 (수동 커밋 제어 권장)
   * @param diffs      비교 결과 스트림 (Iterator)
   * @param batchSize  한 번에 커밋할 배치 크기
   */
  def applyChanges(plan: ComparePlan, diffs: Iterator[DiffRow], targetConn: Connection, batchSize: Int = 512)(
    updateState: (Long, Long, Long, Long, Boolean) => Unit = showUpdateState): Unit = {

    val insStmt = targetConn.prepareStatement(plan.insertSql)
    val updStmt = targetConn.prepareStatement(plan.updateSql)
    val delStmt = targetConn.prepareStatement(plan.deleteSql)

    targetConn.setAutoCommit(false)
    var insCount, updCount, delCount, skiCount = 0L

    try {
      diffs.foreach {
        // insert -----------
        case OnlyInA(keys, sourceVals) =>
          bindRow(insStmt, keys ++ sourceVals, plan.keyIndices ++ plan.valIndices)
          insStmt.addBatch()
          insCount += 1
          if (insCount % batchSize == 0) {
            insStmt.executeBatch()
            updateState(insCount, updCount, delCount, skiCount, false)
          }

        // update -----------
        case Update(keys, sourceVals) =>
          val indices = plan.valIndices ++ plan.keyIndices
          println(indices.mkString("Update Index: ", " ,", ""))
          bindRow(updStmt, sourceVals ++ keys, indices)
          updStmt.addBatch()
          updCount += 1
          if (updCount % batchSize == 0) {
            updStmt.executeBatch()
            updateState(insCount, updCount, delCount, skiCount, false)
          }

        // delete -----------
        case OnlyInB(keys) =>
          bindRow(delStmt, keys, plan.keyIndices)
          delStmt.addBatch()
          delCount += 1
          if (delCount % batchSize == 0) {
            delStmt.executeBatch()
            updateState(insCount, updCount, delCount, skiCount, false)
          }

        case Same(_) =>
          skiCount += 1
          if (skiCount % batchSize == 0) {
            updateState(insCount, updCount, delCount, skiCount, false)
          }
      }

      // 4. 잔여 배치 최종 릴리즈
      if (insCount % batchSize != 0) insStmt.executeBatch()
      if (updCount % batchSize != 0) updStmt.executeBatch()
      if (delCount % batchSize != 0) delStmt.executeBatch()

      targetConn.commit()
      updateState(insCount, updCount, delCount, skiCount, true)

      println(s"[DR Apply Success] Inserted: $insCount, Updated: $updCount, Deleted: $delCount")

    } catch {
      case e: Exception =>
        targetConn.rollback()
        println(s"[DR Apply Failed] Transaction rolled back. Reason: ${e.getMessage}")
        throw e
    } finally {
      insStmt.close()
      updStmt.close()
      delStmt.close()
    }
  }

  private def bindRow(stmt: PreparedStatement, values: List[CVal], indices: List[Int]): Unit = {

    // local mutate for performance
    val vals = values.toIndexedSeq

    var paramIdx = 1
    val it = indices.iterator

    while (it.hasNext) {
      val idx = it.next()
      val cval = vals(idx-1)

      cval match {
        case a: CVOrderable => a match {
          case CInt(_, value) => stmt.setInt(paramIdx, value)
          case CLong(_, value) => stmt.setLong(paramIdx, value)
          case CBigInt(_, value) => stmt.setBigDecimal(paramIdx, new java.math.BigDecimal(value.bigInteger))
          case CDouble(_, value) => stmt.setDouble(paramIdx, value)
          case CDecimal(_, value) => stmt.setBigDecimal(paramIdx, value.bigDecimal)
          case CString(_, value) => stmt.setString(paramIdx, value)
          case CDate(_, value) => stmt.setDate(paramIdx, java.sql.Date.valueOf(value))
          case CTime(_, value) => stmt.setTime(paramIdx, java.sql.Time.valueOf(value))
          case CTimestamp(_, value) => stmt.setTimestamp(paramIdx, java.sql.Timestamp.valueOf(value))

          // todo :: check
          case COffsetTime(_, value) => stmt.setObject(paramIdx, value)
          case COffsetTimestamp(_, value) => stmt.setObject(paramIdx, value)
        }
        case a: CVEquatable => a match {
          case CBoolean(_, value) => stmt.setBoolean(paramIdx, value)
          case CBytes(_, value) => stmt.setBytes(paramIdx, value)

          // todo :: check
          case x: CLongString => stmt.setString(paramIdx, x.longString.makeString)
          case x: CLongBytes => stmt.setBytes(paramIdx, x.longBytes.makeBytes)

          case x: COraGeometry =>
            val targetConn = stmt.getConnection
            val oracleStruct: java.sql.Struct = oracle.spatial.geometry.JGeometry.store(x.value, targetConn)
            stmt.setObject(paramIdx, oracleStruct)

          case CInterval(_, value) => stmt.setString(paramIdx, value) // 오라클 INTERVAL 규격 문자열
          case CXML(_, value) => stmt.setString(paramIdx, value) // XML String
        }

        case a: CVIncomparable => a match {
          case CRowID(_, value) => stmt.setString(paramIdx, value) // 오라클 ROWID는 내부적으로 문자열
          case CBFile(_, value) => stmt.setString(paramIdx, value) // BFile 주소 문자열
          case CNull(_) => stmt.setNull(paramIdx, Types.NULL)

          // this may not happen
          case CNotSupport(_, jdbcType) => stmt.setNull(paramIdx, jdbcType)
        }
      }
      paramIdx += 1
    }
  }

}
