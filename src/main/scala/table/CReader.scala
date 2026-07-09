package table

import table.CReader.getAsString
import table.LongCollection.LongBytes.fromInputStream
import table.LongCollection.LongString.fromReader
import utils.LogHelper._
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.sql.{ResultSet, ResultSetMetaData}
import java.time._

case class CReader(
                   index: Int,                // of resultSet (1-based)
                   name: String,              // for reporting
                   jdbcType: Int,             // java.sql.Types
                   typeName: String,          // DB's type-name
                   precision: Int,            // for Numeric type
                   scale: Int,                // for Numeric type
                   isNullable: Boolean,
                   isVirtual: Boolean = false )
{
  override def toString: String
  = s"index($index):" +
    s"name($name)" +
    s": $typeName($jdbcType: $precision, $scale) nullable($isNullable)"

  private val getter: ResultSet => CVal = CReader.Registry.getOrElse(jdbcType, getAsString _)(this)
  @inline def read(rs: ResultSet): CVal = getter(rs)
}

object CReader {

  implicit val jsonCodec: JsonCodec[CReader] = DeriveJsonCodec.gen[CReader]


  def apply(meta: ResultSetMetaData, databaseProductName: String)(index: Int): CReader = {
    CReader(
      index               = index,
      name                = meta.getColumnName(index),
      jdbcType            = meta.getColumnType(index),
      typeName            = meta.getColumnTypeName(index),
      precision           = meta.getPrecision(index),
      scale               = meta.getScale(index),
      isNullable          = meta.isNullable(index) != ResultSetMetaData.columnNoNulls,
    )
  }

  def getColMetas(rs: ResultSet, databaseProductName: String) = {

    val meta = rs.getMetaData
    val count = meta.getColumnCount
    val cols = (1 to count)
      .map( i => i -> CReader(meta, databaseProductName)(i)).toMap
    cols
  }

  import oracle.jdbc.OracleTypes

  import java.sql.ResultSet
  import java.time.OffsetDateTime // oracle driver

  private val handleNumber: CReader => (ResultSet => CVal) = cs => {
    if (cs.scale == 0) {
      if (cs.precision < 10) getAsInt(cs)
      else if (cs.precision < 19) getAsLong(cs)
      else getAsBigInt(cs)
    } else getAsDecimal(cs)
  }

  private val handleNotSupport: CReader => (ResultSet => CVal) = cs => _ => {
    CNotSupport(cs.index, cs.jdbcType)
  }

  private val handleFloat: CReader => (ResultSet => CVal) = cs => rs => {
    val v = rs.getFloat(cs.index)
    if (cs.isNullable && rs.wasNull()) CNull(cs.index) else CDouble(cs.index, v.toDouble)
  }

  private val handleStruct: CReader => (ResultSet => CVal) = cs => {
    if (cs.typeName.contains("GEOMETRY")) getAsOraGeometry(cs)
    else getAsString(cs)
  }

  // 2. 타입 코드와 Getter 함수 매핑 (HashMap)
  private val Registry: Map[Int, CReader => (ResultSet => CVal)] = Map(
    // Orderable
    OracleTypes.NUMBER          -> handleNumber,
    OracleTypes.NUMERIC         -> handleNumber,
    OracleTypes.DECIMAL         -> handleNumber,
    OracleTypes.INTEGER         -> getAsInt,
    OracleTypes.BIGINT          -> getAsLong,
    OracleTypes.SMALLINT        -> getAsInt,
    OracleTypes.TINYINT         -> getAsInt,
    OracleTypes.BIT             -> getAsInt,

    OracleTypes.BINARY_DOUBLE   -> getAsDouble,   // <<<
    OracleTypes.DOUBLE          -> getAsDouble,   // <<<
    OracleTypes.BINARY_FLOAT    -> handleFloat,
    OracleTypes.FLOAT           -> handleFloat,
    OracleTypes.REAL            -> getAsDouble,   // <<<

    OracleTypes.VARCHAR         -> getAsString,
    OracleTypes.CHAR            -> getAsString,
    OracleTypes.FIXED_CHAR      -> getAsString,
    OracleTypes.NVARCHAR        -> getAsString,
    OracleTypes.NCHAR           -> getAsString,

    OracleTypes.LONGVARCHAR     -> getAsLongString,
    OracleTypes.LONGNVARCHAR    -> getAsLongString,

    OracleTypes.DATE            -> getAsTimestamp,  // 91이지만 오라클은 보통 93으로 처리

    OracleTypes.TIMESTAMP       -> getAsTimestamp,
    OracleTypes.TIMESTAMPTZ     -> getAsOffsetDateTime,
    OracleTypes.TIMESTAMPLTZ    -> getAsTimestamp, //getAsOffsetDateTime,

    // Equatable
    OracleTypes.BOOLEAN         -> getAsBoolean,

    OracleTypes.CLOB            -> getLobAsLongString,
    OracleTypes.NCLOB           -> getLobAsLongString,
    OracleTypes.BLOB            -> getLobAsLongBytes,
    OracleTypes.RAW             -> getAsBytes,
    OracleTypes.BINARY          -> getAsBytes,     // RAW(-2)와 동일
    OracleTypes.VARBINARY       -> getAsBytes,
    OracleTypes.LONGVARBINARY   -> getAsLongBytes, // LONG RAW 대응

    OracleTypes.SQLXML          -> getAsXML,
    OracleTypes.INTERVALYM      -> getAsInterval,
    OracleTypes.INTERVALDS      -> getAsInterval,
    OracleTypes.STRUCT          -> handleStruct,
    OracleTypes.NULL            -> (cs => _ => CNull(cs.index)),

    // Incomparable
    OracleTypes.ROWID           -> getAsRowID,
    OracleTypes.BFILE           -> getAsBFile,
    OracleTypes.VECTOR          -> handleNotSupport,
    OracleTypes.VECTOR_INT8     -> handleNotSupport,
    OracleTypes.VECTOR_FLOAT32  -> handleNotSupport,
    OracleTypes.VECTOR_FLOAT64  -> handleNotSupport,
    OracleTypes.VECTOR_BINARY   -> handleNotSupport,
    OracleTypes.JSON            -> handleNotSupport, // 필요시 getAsString으로 변경 가능
    OracleTypes.CURSOR          -> handleNotSupport,
    OracleTypes.REF_CURSOR      -> handleNotSupport,
    OracleTypes.ARRAY           -> handleNotSupport,
    OracleTypes.REF             -> handleNotSupport,
    OracleTypes.PLSQL_BOOLEAN   -> handleNotSupport,
    OracleTypes.PLSQL_INDEX_TABLE -> handleNotSupport,
    OracleTypes.OPAQUE          -> handleNotSupport,
    OracleTypes.JAVA_OBJECT     -> handleNotSupport,
    OracleTypes.JAVA_STRUCT     -> handleNotSupport
  )

  // --------------------------------------------------------------------------------
  def getAsBFile(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getString(cs.index)
    if (v == null) CNull(cs.index) else CBFile(cs.index, v)
  }

  def getAsInterval(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getString(cs.index)
    if (v == null) CNull(cs.index) else CInterval(cs.index, v)
  }

  def getAsXML(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getString(cs.index)
    if (v == null) CNull(cs.index) else CXML(cs.index, v)
  }

  def getAsInt(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getInt(cs.index)
    if (cs.isNullable && rs.wasNull()) CNull(cs.index) else CInt(cs.index, v)
  }

  def getAsLong(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getLong(cs.index)
    if (cs.isNullable && rs.wasNull()) CNull(cs.index) else CLong(cs.index, v)
  }

  def getAsDecimal(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getBigDecimal(cs.index)
    if (v == null) CNull(cs.index) else CDecimal(cs.index, BigDecimal(v))
  }

  def getAsBigInt(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getBigDecimal(cs.index)
    if (v == null) CNull(cs.index) else CBigInt(cs.index, v.toBigInteger)
  }


  def getAsString(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getString(cs.index)
    if (v == null) CNull(cs.index) else CString(cs.index, v)
  }

  // 오라클 DATE는 시분초를 포함: LocalDateTime(CTimestamp)으로 읽어야 함.
  def getAsTimestamp(cs: CReader): ResultSet => CVal = rs => {

    val t = rs.getObject(cs.index, classOf[LocalDateTime])
    if(t == null) CNull(cs.index) else CTimestamp(cs.index, t)
//    val ts = rs.getTimestamp(cs.index)
//    if (ts == null) CNull(cs.index) else CTimestamp(cs.index, ts.toLocalDateTime)
  }

  // --- 동등성 비교 (CVEquatable) ---
  def getAsBoolean(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getBoolean(cs.index)
    if (cs.isNullable && rs.wasNull()) CNull(cs.index) else CBoolean(cs.index, v)
  }

  def getAsBytes(cs: CReader): ResultSet => CVal = rs => {
    val b = rs.getBytes(cs.index)
    if (b == null) CNull(cs.index) else CBytes(cs.index, b)
  }

  def getLobAsLongString(cs: CReader): ResultSet => CVal = rs => {
    val lob = rs.getClob(cs.index)
    CLongString(cs.index, fromReader(() => lob.getCharacterStream()), None)
  }

  def getAsRowID(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getString(cs.index)
    if (v == null) CNull(cs.index) else CRowID(cs.index, v)
  }

  def getAsOraGeometry(cs: CReader): ResultSet => CVal = rs => {

    val bytes = rs.getBytes(cs.index)
    if (bytes == null) CNull(cs.index) else {
      val geom = oracle.spatial.geometry.JGeometry.load(bytes)
      COraGeometry(cs.index, geom)
    }
  }
  def getAsDouble(cs: CReader): ResultSet => CVal = rs => {
    val v = rs.getDouble(cs.index)
    if (cs.isNullable && rs.wasNull()) CNull(cs.index) else CDouble(cs.index, v)
  }

  // --- 시각  (Oracle 특화) ---
  // 오라클 Date는  hh:mm:ss 를 포함함. Oracle DATE에서 시간 제외하고 날짜만 필요할 때 사용
  def getAsDate(cs: CReader): ResultSet => CVal = rs => {
    mayNot(s"unrecognized type: $cs")(rs.getObject(cs.index, classOf[LocalDate]))
      .map(s => CDate(cs.index, s))
      .getOrElse(CNull(cs.index))
  }

  // TIMESTAMP WITH TIME ZONE 대응
  def getAsOffsetDateTime(cs: CReader): ResultSet => CVal = rs => {
    mayNot(s"unrecognized type: $cs")(rs.getObject(cs.index, classOf[OffsetDateTime]))
      .map(s => COffsetTimestamp(cs.index, s))
      .getOrElse(CNull(cs.index))
  }

  // LONG 타입이나 직접 스트림 읽기 시 사용
  def getAsLongString(cs: CReader): ResultSet => CVal = rs => {
    CLongString(cs.index, fromReader(() => rs.getCharacterStream(cs.index)), None)
  }

  // LONG RAW
  def getAsLongBytes(cs: CReader): ResultSet => CVal = rs => {
    CLongBytes( cs.index, fromInputStream(() => rs.getBinaryStream(cs.index)), None)
  }

  //
  def getLobAsLongBytes(cs: CReader): ResultSet => CVal = rs => {
    val lob = rs.getBlob(cs.index)
    if (lob == null) CNull(cs.index)
    else
      CLongBytes( cs.index, fromInputStream(() => lob.getBinaryStream()), None)
  }

  /*
   * 오라클 DB 자체에는 "날짜 정보가 없는 순수 시간(TIME) 타입"이 존재하지 않음
   * 오라클 DATE: 이름은 Date지만 사실 '날짜 + 시분초' (CTimestamp가 처리)
   * 오라클 TIMESTAMP: '날짜 + 시분초 + 소수점 초' (CTimestamp가 처리)
   * 오라클 INTERVAL DAY TO SECOND: 시간의 양(Duration)을 나타내며, 보통 문자열(CInterval)로 처리
   * 따라서 일반적인 오라클 테이블 비교에서는 getAsTime이나 getAsOffsetTime이 호출될 일이 없음
   */
  def getAsTime(cs: CReader): ResultSet => CVal
  = rs => {
    mayNot(s"unrecognized type, i'll set as Null : $cs")(rs.getObject(cs.index, classOf[LocalTime]) )
      .map(s => CTime(cs.index, s))
      .getOrElse(CNull(cs.index))
  }

  def getAsOffsetTime(cs: CReader): ResultSet => CVal
  = rs => {
    mayNot(s"unrecognized type, i'll set as Null : $cs")( rs.getObject(cs.index, classOf[OffsetTime]) )
      .map(s => COffsetTime(cs.index,s))
      .getOrElse(CNull(cs.index))
  }
}