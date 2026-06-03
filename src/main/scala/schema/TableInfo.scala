package schema

import utils.Implicits.ResultSetIter
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.sql.{Connection, DatabaseMetaData}
import javax.sql.DataSource

case class KeyCol(name: String, cols: List[String]) {
  override def toString: String = cols.mkString(s"$name {", ",", "}")
}

object KeyCol {
  implicit val jsonCodec: JsonCodec[KeyCol] = DeriveJsonCodec.gen[KeyCol]
}

case class TableInfo(name: String,
                     schema: Option[String],
                     cols: List[ColInfo],
                     primaryKey: Option[KeyCol],
                     uniqueKeys: Set[KeyCol],
                     countA: Option[Long] = None,
                     countB: Option[Long] = None,
                    )
{
  val hasKey = primaryKey.isDefined || uniqueKeys.nonEmpty
  val hasUnsortable = cols.exists(_.isUnsortable)
  val hasLob        = cols.exists(_.isLob)
  val hasDateTime   = cols.exists(_.isDateTime)

  override def toString: String
  = s"$name (${cols.size})" + primaryKey.mkString + uniqueKeys.mkString( " uniqueKeys: { ", ", ", "}")

  def getNotAllowed(cols: Set[ColInfo]) = {
    val unsortableTypes = Set( "clob", "blob", "long", "raw", "json", "xmltype", "bfile") // "text", "bytea"
    cols.filter(c => unsortableTypes.contains(c.typeName.toLowerCase))
  }

  def fetchCount(ds1: DataSource, ds2: DataSource): TableInfo = {
    val countA = TableInfo.fetchCount(ds1, this)
    val countB = TableInfo.fetchCount(ds2, this)
    copy(countA= countA, countB = countB)
  }
}

object TableInfo {

  implicit val jsonCodec: JsonCodec[TableInfo] = DeriveJsonCodec.gen[TableInfo]

  def apply(conn: Connection, schema: String, table: String): TableInfo = {

    val meta = conn.getMetaData

    val pkMap = getPrimaryKeyMap(meta, schema, table)
    val cols = getColumns(meta, schema, table, pkMap)
    val pkCols = pkMap.toList.sortBy(_._2).map(_._1)

    val primaryKey = if (pkCols.nonEmpty) Some(KeyCol("PRIMARY", pkCols)) else None

    val uniqueKeys = getUniqueKeys(meta, schema, table)
      .filterNot(uk => primaryKey.exists(_.cols == uk.cols))

    new TableInfo(table, Option(schema), cols, primaryKey, uniqueKeys)
  }


  private def getPrimaryKeyMap(meta: DatabaseMetaData, schema: String, table: String): Map[String, Int] = {
    meta.getPrimaryKeys(null, schema, table).mapIter { r =>
      r.getString("COLUMN_NAME") -> r.getInt("KEY_SEQ")
    }.toMap
  }

  private def getColumns(meta: DatabaseMetaData, schema: String, table: String, pkMap: Map[String, Int]): List[ColInfo] = {
    meta.getColumns(null, schema, table, "%").mapIter { r =>
      val name = r.getString("COLUMN_NAME")
      ColInfo(
        databaseProductName = "Oracle",
        name        = name,
        schemaName  = Option(schema),
        jdbcType    = r.getInt("DATA_TYPE"),
        typeName    = r.getString("TYPE_NAME"),
        precision   = r.getInt("COLUMN_SIZE"),
        scale       = r.getInt("DECIMAL_DIGITS"),
        isNullable  = r.getString("IS_NULLABLE") == "YES",
        ordinalPos  = r.getInt("ORDINAL_POSITION"),
        pkOrdinal   = pkMap.get(name)
      )
    }.toList.sortBy(_.ordinalPos)
  }

  private def getUniqueKeys(meta: DatabaseMetaData, schema: String, table: String): Set[KeyCol] = {
    meta.getIndexInfo(null, schema, table, true, false).mapIter { r =>
        (r.getString("INDEX_NAME"), r.getInt("ORDINAL_POSITION"), r.getString("COLUMN_NAME"))
      }
      .filter(_._1 != null).toList
      .groupBy(_._1)
      .map { case (name, cols) =>
        KeyCol(name, cols.sortBy(_._2).map(_._3))
      }.toSet
  }

  def fetchCount(ds: DataSource, tableInfo: TableInfo) = {

    val conn = ds.getConnection
    val fullTableName = tableInfo.schema.map(s => s"$s.${tableInfo.name}").getOrElse(tableInfo.name)
    val sql = s"SELECT COUNT(*) FROM $fullTableName"
    val stmt = conn.createStatement()
    try {
      val rs = stmt.executeQuery(sql)
      val count = if (rs.next()) rs.getLong(1) else 0L
      Some(count)
    }
    catch { case e: Throwable =>
      None
    } finally {
      stmt.close()
      conn.close()
    }
  }
}


