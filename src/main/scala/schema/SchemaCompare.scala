package schema

import schema.SchemaCompared.tableOfInfos
import tui.SyncTUI.bullet
import tui.layoutzEx._
import utils.Implicits.ResultSetIter
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.sql.{Connection, DatabaseMetaData}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

// ================================================================================
case class SchemaCompared( conf1: DBConf,
                           conf2: DBConf,
                           comparable: List[TableInfo],              // both have same structure
                           mismatchKey: List[(TableInfo, TableInfo)],
                           mismatchCols: List[(TableInfo, TableInfo)],
                           onlyInDb1: List[TableInfo],
                           onlyInDb2: List[TableInfo],
                           comparePlans: List[ComparePlan],
                         ) {
  def get(tableName: String): Option[TableInfo] = comparable.find( _.name == tableName)
  def filterNoKey = comparable.filter(i => i.primaryKey.isEmpty && i.uniqueKeys.isEmpty)
  def filterKey = comparable.filter(i => i.primaryKey.isDefined || i.uniqueKeys.nonEmpty)

  def show_mka(callback: String=> Unit)  = callback(tableOfInfos(mismatchKey.map(_._1)))
  def show_mkb(callback: String=> Unit)  = callback(tableOfInfos(mismatchKey.map(_._2)))
  def show_mca(callback: String=> Unit)  = callback(tableOfInfos(mismatchCols.map(_._1)))
  def show_mcb(callback: String=> Unit)  = callback(tableOfInfos(mismatchCols.map(_._2)))
  def show_oa (callback: String=> Unit)  = callback(tableOfInfos(onlyInDb1))
  def show_ob (callback: String=> Unit)  = callback(tableOfInfos(onlyInDb2))
  def show_ln (callback: String=> Unit)  = callback(tableOfInfos(filterNoKey))
  def show_lk (callback: String=> Unit)  = callback(tableOfInfos(filterKey))
  def show_l  (callback: String=> Unit)  = {
    conf1.display("source", callback)
    conf1.display("target", callback)
    callback(tableOfInfos(comparable))
  }




}

object SchemaCompare {

  implicit val jsonCodec: JsonCodec[SchemaCompared] = DeriveJsonCodec.gen[SchemaCompared]

  private def getTableNames(schema: String, meta: DatabaseMetaData) = {
    meta.getTables(null, schema, "%", Array("TABLE")).mapIter { r =>
      r.getString("TABLE_NAME")
    }.toList
  }

  import java.util.concurrent.Semaphore
  import javax.sql.DataSource
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}

  def fetchTableNames(ds: DataSource, schema: String) = {
    try{
      val conn = ds.getConnection
      try{
        val meta = conn.getMetaData
        val tableNames = getTableNames(schema, meta)
        tableNames
      } finally {
        conn.close()
      }
    }
  }

  def fetchSchema(ds: DataSource, schema: String, tableNames: Seq[String], callback: String => Unit, parallelism: Int = 4)
  : Map[String, TableInfo] = {

    val semaphore = new Semaphore(parallelism)
    val total = tableNames.size
    val counter = new AtomicInteger(0)

    val futures = tableNames.map { tableName =>
      Future {
        semaphore.acquire()
        try {
          val conn = ds.getConnection
          try {
            val current = counter.incrementAndGet()
            callback(s"$current/$total $tableName..")
            val ret = tableName -> TableInfo.apply0(conn, schema, tableName)
            callback(s"$current/$total $tableName")
            ret
          } finally {
            conn.close()
          }
        } finally {
          semaphore.release()
        }
      }
    }
    Await.result(Future.sequence(futures), 60.minutes).toMap
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
  def fetchSchema0(ds: DataSource, schema:String, callback: String => Unit): Map[String, TableInfo] = {

    // todo :: exception
    val conn = ds.getConnection
    val meta: DatabaseMetaData = conn.getMetaData
    val tableNames = getTableNames(schema,meta) // todo :: oracle specific
    conn.close()

    val batches = tableNames.grouped(4).toList

    import scala.concurrent.ExecutionContext.Implicits.global
    val results: List[(String, TableInfo)] = batches.flatMap { batch =>
      val futures = batch.map { tableName =>
        Future {
          val conn = ds.getConnection
          try {
            callback(s".$tableName")
            val ret = tableName -> TableInfo.apply0(conn, schema, tableName)
            callback(s"..$tableName")
            ret
          }
          finally {
            conn.close()
          }
        }
      }
      Await.result(Future.sequence(futures), 60.minutes)
    }

    results.toMap
  }


  def compareSchemas(conf1: DBConf,
                     conf2: DBConf,
                      schema1: Map[String, TableInfo],
                     schema2: Map[String, TableInfo],
                     callback: String => Unit): SchemaCompared = {

    val names1 = schema1.keySet
    val names2 = schema2.keySet

    val commonNames = names1.intersect(names2)

    val identical = mutable.ListBuffer[TableInfo]()
    val keyMismatch = mutable.ListBuffer[(TableInfo, TableInfo)]()
    val columnMismatch = mutable.ListBuffer[(TableInfo, TableInfo)]()

    commonNames.foreach { name =>
      callback(name)
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
    callback("make compare plan")
    val comparePlans = comparable.map(c => ComparePlan.apply(c, false))
    SchemaCompared(
      conf1.withoutPass,
      conf2.withoutPass,
      comparable,
      keyMismatch.toList.sortBy(_._1.name),
      columnMismatch.toList.sortBy(_._1.name),
      (names1 -- names2).toList.sorted.map(schema1),
      (names2 -- names1).toList.sorted.map(schema2),
      comparePlans
    )
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
      t.primaryKey.map(_.toString.color(Color.Green))
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

  def headerAndRows(l: Seq[TableInfo]) = {
    val rows: Seq[Seq[Element]] = l.map(t => rowElements(t) )
    val hdr = Seq("Table", "Key","Col#", "#Source", "$Target")

    hdr -> rows
  }

  def tableOfInfos(l: Seq[TableInfo]): String = {
    if(l.isEmpty) return (bullet + "empty")

    val (hdr, rows) = headerAndRows(l)
    val str = table( hdr, rows )
    str.render
  }

  def selectTables(l: Seq[TableInfo])
                  (implicit term: Terminal, screenSemaphore: ScreenSemaphore): Seq[TableInfo] = {
    if(l.isEmpty)
      return List.empty

    val (hdr, rows) = headerAndRows(l)
    val ss = MultiTable
      .multiTable("select tables",  hdr, rows)
      .run( clearOnStart=false, clearOnExit= false, terminal= Some(term))
      .map(_.selected).getOrElse(Set.empty)

    val tables = l.zipWithIndex.flatMap{ case (a, i) => if(ss.contains(i)) Some(a) else None }
    tables
  }
}
