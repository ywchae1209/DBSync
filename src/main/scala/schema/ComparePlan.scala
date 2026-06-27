package schema

import schema.ComparePlan.{CancelledException, cancelleableIt}
import table._
import tui.{Aborted, HasName, ReportMsg, TUITask}
import tui.{Aborted, Finished, HasName, InProgress, ReportMsg, Stopped, TUITask, TaskStatus}
import zio.json.{DeriveJsonCodec, JsonCodec}
import tui.layoutzEx._

import java.time.LocalDateTime
import javax.sql.DataSource

case class ComparePlan( table: TableInfo, // TableInfo,
                        sourceSql: String,
                        targetSql: String,
                        sourceReaders: List[CReader],
                        targetReaders: List[CReader],
                        compRow: CompRow,
                        useLOBHash: Boolean = false ) extends HasName { self =>
  val name: String = table.name

  def toCompareApplyTask(s1: DataSource, s2: DataSource, mock: Boolean): TUITask = {

    new TUITask(name) {
      override def go(cancel: () => Boolean, notice: ReportMsg => Unit): Unit = {

        val reportIt = makeReportIt(notice, false)
        val reportAp = makeReportAp(notice, false)
        val comp = new TableComparer(self, false)

        val it0 = comp.compareIt(s1, s2, reportIt)
        val it = cancelleableIt(it0, cancel, None)
        val filtered = it.filterNot(_.isInstanceOf[Same])
        val con = if(mock) new Mockup.LoggingConnection else s2.getConnection

        try{
          TableComparer.applyChanges( self, filtered,
            con,
            reportAp,
            debug= false)
        } finally {
          con.close()
          it0.close()
        }
      }
    }
  }

  def toCompareToFile(s1: DataSource, s2: DataSource, path: String): TUITask = {

    import DiffRowSerDe.writeDiffRows

    new TUITask(name) {
      override def go(cancel: () => Boolean, notice: ReportMsg => Unit): Unit = {

        val reportIt = makeReportIt(notice, false)
        val comp = new TableComparer(self, false)

        val it0 = comp.compareIt(s1, s2, reportIt)
        val it = cancelleableIt(it0, cancel, None)
        val filtered = it.filterNot(_.isInstanceOf[Same])
        try{
          val done = writeDiffRows(name, path, filtered, cancel, notice)
          done match {
            case Left(e) => notice(ReportMsg(name, s"[Write Abort] ${e.getMessage}", Aborted))
            case Right(l) => notice(ReportMsg(name, s"[Write Done] diff.total = $l", Finished))
          }
        } finally {
          it0.close()
        }
      }
    }
  }

  def toApplyFromFile(s2: DataSource, path: String): TUITask = {

    import DiffRowSerDe.readDiffRows

    new TUITask(name) {
      override def go(cancel: () => Boolean, notice: ReportMsg => Unit): Unit = {

        val reportAp = makeReportAp(notice, false)
        val con = s2.getConnection
        try{
          val it0 = readDiffRows(name, path, cancel, notice)
          val done = it0.map( it =>
            TableComparer.applyChanges( self, it, con, reportAp, debug= false)
          )
          done match {
            case Left(e) => notice(ReportMsg(name, s"[Write Abort] ${e.getMessage}", Aborted))
            case Right(l) => notice(ReportMsg(name, s"[Write Done] diff.total = $l", Finished))
          }
        } finally {
          con.close()
        }
      }
    }

  }

  def makeReportIt(notice: ReportMsg => Unit, verbose: Boolean)
  =
    (r1:Long, r2:Long, s:Long, u:Long, a:Long, b:Long, m: String, fin: Boolean) => {
      val msg =
        if (fin) s"match : $m ra:$r1 rb:$r2 sa:$s ".color(Color.Yellow).render + s"up:$u oa:$a ob:$b".color(Color.Green).render
        else s"match : $m ra:$r1 rb:$r2 sa:$s up:$u oa:$a ob:$b"

      val rm = ReportMsg(name, msg, if (fin) Finished else InProgress)
      notice(rm)
      if(verbose) println(rm.statusString)
    }

  def makeReportAp(notice: ReportMsg => Unit, verbose: Boolean)
  = (ic: Long, uc: Long, dc: Long, sc: Long, fin: Boolean) => {
    val msg =
      if(fin) (s"apply :  in:$ic up:$uc de:$dc sa:$sc".color(Color.Yellow).render)
      else    (s"apply :  in:$ic up:$uc de:$dc sa:$sc")

    val rm = ReportMsg(name, msg, if (fin) Finished else InProgress)
    notice(rm)
    if(verbose) println(rm.statusString)
  }


  def goCompare(s1: DataSource, s2: DataSource,
                limit: Option[Int],
                cancel: () => Boolean,
                notice: ReportMsg => Unit,
                verbose: Boolean= true,
                compDebug: Boolean = false): Unit = {

    val reportIt: (Long, Long, Long, Long, Long, Long, String, Boolean) => Unit =
      makeReportIt(notice, verbose)

    val comp = new TableComparer(this, compDebug)
    val it0 = comp.compareIt(s1, s2, reportIt)
    val it = cancelleableIt(it0, cancel, limit)
    try {
      while (it.hasNext) {
        it.next()
      }
    } catch {
      case e: CancelledException => reportIt(0,0,0,0,0,0, "cancelled at " + e.at.toString, true)
      case e: Throwable => reportIt(0,0,0,0,0,0, s"failed: ${e.getMessage}", true)
        throw e
    } finally {
      it0.close()
    }
  }

  def goCompareApply(s1: DataSource,
                     s2: DataSource,
                     limit: Option[Int],
                     cancel: () => Boolean,
                     notice: ReportMsg => Unit,
                     verbose: Boolean= true,
                     compDebug: Boolean = false,
                     applDebug: Boolean = false) = {

    val reportIt = makeReportIt(notice, verbose)
    val reportAp = makeReportAp(notice, verbose)

    val comp = new TableComparer(this, compDebug)
    val it0 = comp.compareIt(s1, s2, reportIt)
    val it = cancelleableIt(it0, cancel, limit)
    val filtered = it.filterNot(_.isInstanceOf[Same])
    val con = if(applDebug) new Mockup.LoggingConnection else s2.getConnection

    try{
      TableComparer.applyChanges( this, filtered, con,
        reportAp,
        debug= applDebug)
    } finally {
      con.close()
      it0.close()
    }

  }

  val keyComps: List[(CValComp, Int, Int)] = compRow.sortKey.map { k =>
    val rA = sourceReaders.find(_.index == k.colA).get
    val rB = targetReaders.find(_.index == k.colB).get
    val comp = CValComp(rA, rB, k.ascending, k.nullAsSmallest, k.tolerance, equalCheckOnly = false)
    (comp, k.colA, k.colB)
  }

  val colComps: List[(CValComp, Int, Int)] = compRow.compCols.map { c =>
    val rA = sourceReaders.find(_.index == c.colA).get
    val rB = targetReaders.find(_.index == c.colB).get
    val comp = CValComp(rA, rB, ascending = true, nullAsSmallest = false, c.tolerance, equalCheckOnly = true)
    (comp, c.colA, c.colB)
  }

  val lobComps = compRow.compLobs.map{ l =>
    val rA = sourceReaders.find(_.index == l.colA).get
    val rB = targetReaders.find(_.index == l.colA).get
    val comp = CValComp(rA, rB, ascending = true, nullAsSmallest = false, None, equalCheckOnly = true)
    (comp, l.colA, l.colB)
  }

  val (insertSql, updateSql, deleteSql, keyIndices, valIndices) = TableComparer.sqlForApply(this)

}
// --------------------------------------------------------------------------------
final case class CompLOB(name: String,
                         colA: Int,     // LOB Locator index
                         colB: Int)

object CompLOB {
  implicit val jsonCodec: JsonCodec[CompLOB] = DeriveJsonCodec.gen[CompLOB]

}

// --------------------------------------------------------------------------------
final case class CompKey(name: String,
                         colA: Int,
                         colB: Int,
                         ascending: Boolean = true,
                         nullAsSmallest: Boolean = false,
                         tolerance: Option[Tolerance] = None) {
  override def toString: String
  = s"{ colA: $colA, colB: $colB, " +
    s"ascending: $ascending, nullAsSmallest: $nullAsSmallest, ${tolerance.mkString } }"
}

object CompKey {
  implicit val jsonCodec: JsonCodec[CompKey] = DeriveJsonCodec.gen[CompKey]
}

// --------------------------------------------------------------------------------
final case class CompCol(name: String,
                         colA: Int,
                         colB: Int,
                         tolerance: Option[Tolerance],
                         isVirtual: Boolean = false
                        ) {
  override def toString: String = s"{ colA: $colA, colB: $colB, ${tolerance.mkString} }"
}

object CompCol {
  implicit val jsonCodec: JsonCodec[CompCol] = DeriveJsonCodec.gen[CompCol]
}

final case class CompRow(sortKey: List[CompKey],
                         compCols: List[CompCol],
                         compLobs: List[CompLOB] = Nil )
object CompRow {
  implicit val jsonCodec: JsonCodec[CompRow] = DeriveJsonCodec.gen[CompRow]
}

object ComparePlan {

  case class CancelledException(at: LocalDateTime) extends RuntimeException(s"cancelled at $at")

  def cancelleableIt[A](it: Iterator[A] with AutoCloseable,
                        cancel: () => Boolean,
                        limit: Option[Int]
                       ) : Iterator[A]
  = new Iterator[A] {
    val nlimit = limit.getOrElse(0)
    var n = 1
    override def hasNext: Boolean = it.hasNext

    override def next(): A = {
      val needStop = cancel() || (nlimit != 0 && n >= nlimit)

      if(needStop) {
        it.close()
        throw CancelledException(LocalDateTime.now())
      } else {
        n += 1
        it.next()
      }
    }
  }



  implicit val jsonCodec: JsonCodec[ComparePlan] = DeriveJsonCodec.gen[ComparePlan]

  // --------------------------------------------------------------------------------
  private class BuildContext {
    val selectParts = scala.collection.mutable.ListBuffer[String]()
    val orderParts  = scala.collection.mutable.ListBuffer[String]()
    val readers     = scala.collection.mutable.ListBuffer[CReader]()

    val sortKeyPlans = scala.collection.mutable.ListBuffer[CompKey]()
    val compColPlans = scala.collection.mutable.ListBuffer[CompCol]()
    val compLobPlans = scala.collection.mutable.ListBuffer[CompLOB]()

    var currentIndex = 1

    def nextIndex(step: Int = 1): Int = {
      val idx = currentIndex
      currentIndex += step
      idx
    }
  }

  def apply(table: TableInfo, useLOBHash: Boolean): ComparePlan = {
    val ctx = new BuildContext()

    // 1. 정렬 키(Sort Key)로 사용할 컬럼 명단 확정
    val keyCols: Set[String] =
      if (table.primaryKey.isDefined) {
        table.cols.filter(_.pkOrdinal.isDefined).map(_.name).toSet
      } else if (table.uniqueKeys.nonEmpty) {
        table.uniqueKeys.head.cols.toSet
      } else {
        table.cols.filter(_.isSortable).map(_.name).toSet
      }

    // 2. 순서: 정렬 키(0) 일반 컬럼(1) LONG 계열(2) LOB 계열(3)
    // https://www.oracle-developer.net/display.php?id=430
    // 하나의 SQL문에서 LONG (또는 LONG RAW) 타입과 LOB (CLOB, BLOB) 타입을 동시에 조회할 때,
    // LONG 컬럼은 무조건 LOB 컬럼보다 구조상 앞에 위치해야 하고, 더 먼저 읽어야(Fetch) 한다.
    // 만약 LOB 컬럼을 하나라도 먼저 읽는 순간, 뒤에 오는 LONG 컬럼의 데이터 스트림은 드라이버 내부에서 강제로 폐쇄(Close)된다.
    val sortedCols = table.cols.sortBy { c =>
      val typeWeight =
        if (c.typeName.toUpperCase.contains("LONG")) 1 // LONG, LONG RAW
        else if (c.isLob) 3                            // LOB(CLOB/BLOB)
        else 2                                         // 일반 컬럼

      if (table.primaryKey.isDefined) {
        (c.pkOrdinal.getOrElse(Int.MaxValue), typeWeight, c.ordinalPos)
      } else if (keyCols.contains(c.name)) {
        (0, typeWeight, c.ordinalPos)
      } else {
        (1, typeWeight, c.ordinalPos)
      }
    }

    // 3. 컬럼별 처리 진행
    sortedCols.foreach { col =>
      if (col.isLob) processLobColumn(col, ctx, useLOBHash)
      else processRegularColumn(col, ctx, keyCols)
    }

    // 4. SQL 생성
    val sql = buildSql(table, ctx.selectParts.toSeq, ctx.orderParts.toSeq)

    ComparePlan(
      table         = table,
      sourceSql     = sql,
      targetSql     = sql,
      sourceReaders = ctx.readers.toList,
      targetReaders = ctx.readers.toList,
      compRow       = CompRow(ctx.sortKeyPlans.toList, ctx.compColPlans.toList, ctx.compLobPlans.toList)
    )
  }

  /** LOB 컬럼 처리 */
  private def processLobColumn(c: ColInfo, ctx: BuildContext, useLOBHash: Boolean): Unit = {
    val lobIdx  = ctx.nextIndex()

    ctx.selectParts += c.name
    ctx.readers     += createCReader(c, lobIdx, "", c.jdbcType, isVirtual = false)
    ctx.compLobPlans+= CompLOB(c.name, lobIdx, lobIdx)

  }

  /** 일반 컬럼 */
  private def processRegularColumn(c: ColInfo, ctx: BuildContext, keyCols: Set[String]): Unit = {
    val idx = ctx.nextIndex()

    ctx.selectParts += c.name
    ctx.readers += createCReader(c, idx, "", c.jdbcType, isVirtual = false)

    if (keyCols.contains(c.name)) {
      // 정렬 키 그룹에 속한 경우만 ORDER BY 구문 및 CompKey 계획에 추가
      ctx.orderParts   += s"${c.name} ASC"
      ctx.sortKeyPlans += CompKey(c.name, idx, idx)
    } else {
      // 정렬 키가 아니라면 순수 데이터 비교 컬럼(CompCol)으로 분류
      ctx.compColPlans += CompCol(c.name, idx, idx, None)
    }
  }

  private def buildSql(table: TableInfo, selects: Seq[String], orders: Seq[String]): String = {
    val fullTableName = table.schema.map(_ + ".").getOrElse("") + table.name
    val selectClause = selects.mkString(", ")
    val orderClause = if (orders.nonEmpty) s"ORDER BY ${orders.mkString(", ")}" else ""
    s"SELECT $selectClause FROM $fullTableName $orderClause"
  }

  private def createCReader(c: ColInfo, idx: Int, suffix: String, jType: Int, isVirtual: Boolean): CReader = {
    CReader(
      index = idx,
      name = c.name + suffix,
      jdbcType = jType,
      typeName = if(isVirtual) "VIRTUAL" else c.typeName,
      precision = if(isVirtual) 4000 else c.precision,
      scale = if(isVirtual) 0 else c.scale,
      isNullable = c.isNullable,
      isVirtual = isVirtual
    )
  }
  // --------------------------------------------------------------------------------
}
