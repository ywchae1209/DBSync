package schema

import zio.json.{DeriveJsonCodec, JsonCodec}

import java.sql.Types

// from Schema info.
case class ColInfo(
                    databaseProductName: String,
                    schemaName: Option[String],
                    name: String,
                    typeName: String,
                    jdbcType: Int,
                    isNullable: Boolean,
                    ordinalPos: Int,               // 테이블 내 실제 순서
                    precision: Int,                // 전체 자릿수
                    scale: Int,                    // 소수점 자릿수
                    pkOrdinal: Option[Int] = None, // Some(순서)이면 PK
                    isUniqueKey: Boolean = false
                  )
{

  val isLob: Boolean = ColInfo.lobTypes.contains(jdbcType)
  val isDateTime: Boolean = ColInfo.dateTimeTypes.contains(jdbcType)
  val isPrimaryKey: Boolean = pkOrdinal.isDefined
  val isUnsortable = ColInfo.unsortableTypes.contains(jdbcType) || ColInfo.unSortableOra.exists(typeName.toUpperCase.contains)
  val isSortable: Boolean = !isUnsortable

  override def toString: String = {
    val pkMarker = pkOrdinal.map(n => s"[PK$n]").getOrElse("")
    val ukMarker = if (isUniqueKey && !isPrimaryKey) "[UK]" else ""
    s"$pkMarker$ukMarker$typeName($jdbcType)\t$name" +
      (if (isNullable) ":null" else "") +
      s"\tpos$ordinalPos:$precision:$scale"
  }
}

object ColInfo {

  val blobTypes = Set(Types.BLOB)
  val clobTypes = Set(Types.CLOB, Types.NCLOB)
  val lobTypes  = blobTypes ++ clobTypes
  val dateTimeTypes   = Set(Types.DATE, Types.TIMESTAMP, Types.TIMESTAMP_WITH_TIMEZONE)
  val unsortableTypes = blobTypes ++ clobTypes ++ Set(
    Types.LONGVARBINARY, Types.NCLOB, Types.LONGVARCHAR, Types.LONGNVARCHAR,
    Types.BINARY, Types.VARBINARY,
    Types.ARRAY, Types.STRUCT, Types.REF,
    Types.SQLXML, Types.DATALINK
    )

  private val unSortableOra = Set("SDO_GEOMETRY", "XMLTYPE", "BFILE", "ANYDATA")
  implicit val jsonCodec: JsonCodec[ColInfo] = DeriveJsonCodec.gen[ColInfo]
}

