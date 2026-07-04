package table

import schema.ComparePlan
import tui.layoutzEx.StringWithColor
import tui.ReportMsg.noticeWithLog
import tui.TaskStatus._
import tui.{ReportMsg, ReportMsgElapse, ReportMsgIt, ReportMsgTime, TaskStatus}
import utils.LogHelper.{memo, note, oops}

import java.sql.ResultSet
import javax.sql.DataSource


case class RowReadException(msg: String, e: Exception) extends Exception

case class TableComparer(plan: ComparePlan,
                         fetchSize: Int = 256,
                         reportInterval: Int = 512,
                         isDebug: Boolean = false) {

  def debugMemo(s: => String) = if(isDebug) memo(s)

  def compareIt(srcDs: DataSource,
                tgtDs: DataSource,
                notice: ReportMsg => Unit): Iterator[DiffRow] with AutoCloseable = {

    def report(r1:Long, r2:Long, s:Long, u:Long, a:Long, b:Long, m: String, state: TaskStatus)
    = {
      val rm = ReportMsgIt(plan.name, r1, r2, s, u, a, b, m, state)
      noticeWithLog(notice)(rm)
    }

    val srcConn = srcDs.getConnection
    val tgtConn = tgtDs.getConnection

    val srcStmt = srcConn.prepareStatement(plan.sourceSql)
    val tgtStmt = tgtConn.prepareStatement(plan.targetSql)

    srcStmt.setFetchSize(fetchSize)
    tgtStmt.setFetchSize(fetchSize)

    val msgStart = ReportMsgTime(plan.name, "start")
    noticeWithLog(notice)(msgStart)
    def elapseLog(msg: String) = noticeWithLog(notice)(ReportMsgElapse(plan.name, msgStart, msg))

    val srcRs = srcStmt.executeQuery()
    val tgtRs = tgtStmt.executeQuery()

    elapseLog("first result")

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
      @inline private def noticeIt(msg: String = "", state: TaskStatus = InProc): Unit = {
        val sum = readCount1 + readCount2
        if (state.isDone || (sum % reportInterval == 0)) {
          report(readCount1, readCount2, sameCount, updCount, onlyACount, onlyBCount, msgStart.intervalStr(msg), state)
        }
      }

      private var firstRow = true
      private def readRowSequentially(rs: ResultSet, readers: List[CReader], label: String): IndexedSeq[CVal] = {
        debugMemo(s"\n============ [$label] Row Extraction Start ============")
        val extracted = readers.map { r =>
          try {
            val cval = r.read(rs)
            debugMemo(f"[Idx ${r.index}%2d] ${r.name}%-16s (${r.typeName}%-10s) => $cval")
            cval
          } catch {
            case e: Exception =>
              val msg =
                s"[READ ERROR at Idx ${r.index}] Column: ${r.name.green}, Type: ${r.typeName} JDBCType: ${r.jdbcType} " +
                  s" Reason: ${e.getMessage}"

              oops(s"${plan.name.green}" + msg)
              noticeIt( msg, Abort)
              throw RowReadException(msg, e)
          }
        }.toIndexedSeq
        debugMemo(s"============ [$label] Row Extraction End ==============\n")

        if(firstRow) {
          elapseLog("first read")
          firstRow = false
        }

        extracted
      }

      private def advanceSource(): Unit = {
        srcHasNext = srcRs.next()
        readCount1 += 1
        curSrcRow = if (srcHasNext) readRowSequentially(srcRs, sortedSrcReaders, "SOURCE") else null
      }

      private def advanceTarget(): Unit = {
        tgtHasNext = tgtRs.next()
        readCount2 += 1
        curTgtRow = if (tgtHasNext) readRowSequentially(tgtRs, sortedTgtReaders, "TARGET") else null
      }

      override def hasNext: Boolean = {
        try {
          hasNext0
        } catch {
          case e: RowReadException => throw e   // already reported
          case e: Throwable =>
          oops(s"${plan.name} hasNext failed. reason : ${e.toString}")
          noticeIt(s"hasNext failed : ${e.getMessage}")
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
            advanceSource()
            advanceTarget()
            noticeIt()
          } else if (keyDiff < 0) {
            onlyACount += 1
            nextBuffer = Some(OnlyInA(extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            advanceSource()
            noticeIt()
          } else {
            onlyBCount += 1
            nextBuffer = Some(OnlyInB(extractTargetKeys(curTgtRow)))
            advanceTarget()
            noticeIt()
          }
        }

        if (nextBuffer.isEmpty) {
          if (srcHasNext) {
            onlyACount += 1
            nextBuffer = Some(OnlyInA(extractSourceKeys(curSrcRow), extractSourceVals(curSrcRow)))
            advanceSource()
            noticeIt()
          } else if (tgtHasNext) {
            onlyBCount += 1
            nextBuffer = Some(OnlyInB(extractTargetKeys(curTgtRow)))
            advanceTarget()
            noticeIt()
          }
        }

        if (nextBuffer.isEmpty) close0()

        nextBuffer.isDefined
      }

      override def next(): DiffRow = {
        if (!hasNext) {
          close0()
          throw new NoSuchElementException("No more diff data.")
        }
        val res = nextBuffer.get
        nextBuffer = None
        res
      }

      def close0(): Unit = {

        Option(srcRs).foreach(_.close())
        Option(tgtRs).foreach(_.close())
        Option(srcStmt).foreach(_.close())
        Option(tgtStmt).foreach(_.close())
        Option(srcConn).foreach(_.close())
        Option(tgtConn).foreach(_.close())
      }

      override def close(): Unit = {
        noticeIt("$", Fin)
        close0()
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


