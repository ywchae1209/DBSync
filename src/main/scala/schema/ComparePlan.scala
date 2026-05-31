package schema

import table._
import zio.json.{DeriveJsonCodec, JsonCodec}
import javax.sql.DataSource

case class ComparePlan(
                        table: TableInfo, // TableInfo,
                        sourceSql: String,
                        targetSql: String,
                        sourceReaders: List[CReader],
                        targetReaders: List[CReader],
                        compRow: CompRow,
                        useLOBHash: Boolean = false
                      ) {
  def goWith(s1: DataSource, s2: DataSource,
             count: Option[Int],
             applyTarget: Boolean,
             exceptSame: Boolean = false,
             compDebug: Boolean = false,
             applDebug: Boolean = false) = {

    val comp = new TableComparer(this, compDebug)
    val it = comp.compareIt(s1, s2)
    try{
      val sized = if(count.isEmpty) it else it.take(count.get)
      val filtered = if(exceptSame) sized.filterNot(_.isInstanceOf[Same]) else sized
      val logged = filtered.map{ dr =>
        if(compDebug) {
          println( "--- " + dr)
          println( "+++ " + DiffRow.fromBytes(dr.serialize))
        }
        DiffRow.fromBytes(dr.serialize)
      }
      if(applyTarget) {
        val con = s2.getConnection
        TableComparer.applyChanges(this, logged, if(applDebug) new Mockup.LoggingConnection else con)()
        con.close()
      } else
        logged.size
      it.close()

    } catch { case e: Exception =>
      println(e.toString)
      it.close()
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
  override def toString: String
  = s"{ colA: $colA, colB: $colB, ${tolerance.mkString} }"
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
