package schema

import tui.layoutzEx._
import utils.Implicits.ResultSetIter
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.sql.{Connection, DatabaseMetaData}
import javax.sql.DataSource
import scala.collection.mutable
import scala.util.Using

// ================================================================================
case class SchemaCompared(comparable: List[TableInfo],              // both have same structure
                          mismatchKey: List[(TableInfo, TableInfo)],
                          mismatchCols: List[(TableInfo, TableInfo)],
                          onlyInDb1: List[TableInfo],
                          onlyInDb2: List[TableInfo],
                          comparePlans: List[ComparePlan],
                          crytoGrantedA: Boolean = false,
                          crytoGrantedB: Boolean = false,
                         ) {
  def get(tableName: String): Option[TableInfo]
  = comparable.find( _.name == tableName)

  def filterNoKey = comparable.filter(i => i.primaryKey.isEmpty && i.uniqueKeys.isEmpty)
  def filterKey = comparable.filter(i => i.primaryKey.isDefined || i.uniqueKeys.nonEmpty)
  def filterUk = comparable.filter(i => i.uniqueKeys.nonEmpty)

  def updatePlan(useLOBHash: Boolean): SchemaCompared = {
    println("LOB hash not supported.")
 //   this.copy( comparePlans = comparable.map( c => ComparePlan.apply(c, useLOBHash)))
    this
  }
}

object SchemaCompare {

  implicit val jsonCodec: JsonCodec[SchemaCompared] = DeriveJsonCodec.gen[SchemaCompared]

  private def getTableNames(schema: String, meta: DatabaseMetaData) = {
    meta.getTables(null, schema, "%", Array("TABLE")).mapIter { r =>
      r.getString("TABLE_NAME")
    }.toList
  }

  /** {{{
   *  |    DBMS    |   catalog      |  schema                                       |
   *  |:----------:|:--------------:|:---------------------------------------------:|
   *  | Oracle     | NA (use null)  | user-account-name = schema-name               |
   *  | PostgreSQL | DB name        | schema-name (default: public)                 |
   *  | MySQL      | DB name        | Essentially the same as DB name, usually null |
   *  | MSSQL      | DB name        | Schema name (e.g., dbo)         )             |
  }}}
  */
  def fetchSchema(ds: DataSource, schema:String): Map[String, TableInfo] = {

    val conn = ds.getConnection
    val meta: DatabaseMetaData = conn.getMetaData
    val tableNames = getTableNames(schema,meta) // todo :: oracle specific

    tableNames.map { tableName =>
      tableName -> TableInfo(conn, schema, tableName)
    }.toMap
  }

  def cryptoGranted(conn: Connection) = {

    // 오라클 시스템 권한 뷰(ALL_TAB_PRIVS)를 조회하는 쿼리
    // TABLE_NAME은 패키지명이며, PRIVILEGE는 'EXECUTE' 상태인지를 검사합니다.
    val sql =
      """
        |SELECT COUNT(*)
        |FROM ALL_TAB_PRIVS
        |WHERE TABLE_NAME = 'DBMS_CRYPTO'
        |  AND PRIVILEGE = 'EXECUTE'
      """.stripMargin

    val result = Using(conn.prepareStatement(sql)) { stmt =>
      Using(stmt.executeQuery()) { rs =>
        if (rs.next()) {
          rs.getInt(1) > 0
        } else {
          false
        }
      }.getOrElse(false)
    }
    val granted = result.getOrElse(false)
    granted
  }


  def showEncoding(conn: Connection) = {
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery(
      "SELECT PARAMETER, VALUE FROM NLS_DATABASE_PARAMETERS " +
        "WHERE PARAMETER IN ('NLS_CHARACTERSET','NLS_NCHAR_CHARACTERSET','NLS_LANGUAGE')")

    while (rs.next()) {
      System.out.println(rs.getString("PARAMETER") + " = " + rs.getString("VALUE"))
    }
    rs.close()
    stmt.close()
  }

  def compareSchemas(schema1: Map[String, TableInfo],
                     schema2: Map[String, TableInfo]): SchemaCompared = {

    val names1 = schema1.keySet
    val names2 = schema2.keySet

    val commonNames = names1.intersect(names2)

    val identical = mutable.ListBuffer[TableInfo]()
    val keyMismatch = mutable.ListBuffer[(TableInfo, TableInfo)]()
    val columnMismatch = mutable.ListBuffer[(TableInfo, TableInfo)]()

    commonNames.foreach { name =>
      val s1 = schema1(name)
      val s2 = schema2(name)

      if (s1.cols != s2.cols) {
        columnMismatch += (s1 -> s2)
      } else if (s1.primaryKey != s2.primaryKey ) {
        keyMismatch += (s1 -> s2)
      } else {
        identical += s1
      }
    }

    val comparable = identical.toList.sortBy(_.name)
    val comparePlans = comparable.map(c => ComparePlan.apply(c, false))
    SchemaCompared(
      comparable,
      keyMismatch.toList.sortBy(_._1.name),
      columnMismatch.toList.sortBy(_._1.name),
      (names1 -- names2).toList.sorted.map(schema1),
      (names2 -- names1).toList.sorted.map(schema2),
      comparePlans
    )
  }


  def checkGrant(ds1: DataSource, ds2: DataSource, o: SchemaCompared): SchemaCompared = {
    val conn1 = ds1.getConnection
    val conn2 = ds1.getConnection
    try{
      val g1 = cryptoGranted(conn1)
      val g2 = cryptoGranted(conn2)
      o.copy( crytoGrantedA = g1, crytoGrantedB = g2)
    } finally {
      conn1.close()
      conn2.close()
    }
  }

}

//////////////////////////////////////////////////////////////////////////////////

object SchemaCompared {

  def summary(a: SchemaCompared) = {
    val tbl = table(
      Seq("category", "count"),
      Seq(
        Seq("1. Comparable",   a.comparable.size.toString.color(Color.BrightMagenta)),
        Seq("2. MismatchKey",  a.mismatchKey.size.toString),
        Seq("3. MismatchCols", a.mismatchCols.size.toString),
        Seq("4. Only in DB_1", a.onlyInDb1.size.toString),
        Seq("5. Only in DB_2", a.onlyInDb2.size.toString),
      )
    ).style(Style.Bold)

    layout( tbl ).render
  }


  def colsTable(l: List[ColInfo]) = {

    val hdr = Seq("name", "type", "nullable", "precision", "scale")
    val rows = l.map( c =>
      Seq(
        c.name.color(Color.Cyan).render,
        s"${c.typeName}(${c.jdbcType})",
        if(c.isNullable) "Nullable" else "",
        c.precision.toString,
        c.scale.toString,
      ).map( Text)
    )
    hdr -> rows
  }

  def tableDetail(t: TableInfo) = {
    val (hdr, rows) = colsTable(t.cols)
    layout(
      row(
        t.name.style(Style.Reverse),
        Text("key: "),
        t.primaryKey.map(_.toString.color(Color.BrightMagenta))
          .getOrElse(
            t.uniqueKeys
              .headOption.map(_.toString.color(Color.Green))
              .getOrElse("No-Key".color(Color.Cyan))
          ),
        Text(" # "),
        Text( t.cols.size.toString),
        Text( t.countA.map(_.toString).getOrElse("")),
        Text( t.countB.map(_.toString).getOrElse("")),
      ),
      table(hdr, rows)
    ).render

  }

  def textYellow(s: String, when: Boolean): Element = {
    if(when) s.color(Color.Yellow) else s
  }


  def rowElements(t: TableInfo): Seq[Element] = {
    val lob = t.hasLob

    Seq(
      textYellow(t.name, lob),
      t.primaryKey.map(_.toString.color(Color.BrightMagenta))
        .getOrElse(
          t.uniqueKeys
            .headOption.map(_.toString.color(Color.Green))
            .getOrElse("No-Key".color(Color.Cyan))
        ),
      textYellow(t.cols.size.toString, lob),
      Text( t.countA.map(_.toString).getOrElse("")),
      Text( t.countB.map(_.toString).getOrElse(""))
    )
  }

  def headerAndRows(l: List[TableInfo]) = {
    val rows: Seq[Seq[Element]] = l.map(t => rowElements(t) )
    val hdr = Seq("Table", "Key","Col#", "#Source", "$Target")

    hdr -> rows
  }

  def tableOfInfos(l: List[TableInfo]): String = {
    if(l.isEmpty) return "empty"

    val (hdr, rows) = headerAndRows(l)
    val str = table( hdr, rows )
    str.render
  }

}


