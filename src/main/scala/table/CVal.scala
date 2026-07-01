package table

import oracle.spatial.geometry.JGeometry
import oracle.spatial.util.{WKB, WKT}
import table.LongCollection.{LongBytes, LongString}

import java.time._

sealed trait CVal {
  def idx: Int
  val valueStr = toString // for screen display

  override def toString: String = this match {
    case a: CVOrderable => a match {
      case CInt(idx, value)       => s"($idx, $value)"
      case CLong(idx, value)      => s"($idx, $value)"
      case CBigInt(idx, value)    => s"($idx, $value)"
      case CDouble(idx, value)    => s"($idx, $value)"
      case CDecimal(idx, value)   => s"($idx, $value)"
      case CString(idx, value)    => s"($idx, '$value')"
      case CDate(idx, value)      => s"($idx, $value)"
      case CTime(idx, value)      => s"($idx, $value)"
      case CTimestamp(idx, value) => s"($idx, $value)"
      case COffsetTime(idx, value)=> s"($idx, $value)"
      case COffsetTimestamp(idx, value)=> s"($idx, $value)"
    }
    case a: CVEquatable => a match {
      case CBoolean(idx, value)      => s"($idx, $value)"
      case CBytes(idx, value)        => s"($idx, Bytes(${value.length}))"
      case x@COraGeometry(idx, _)    => s"($idx, ${x.valueStr})"
      case CInterval(idx, value)     => s"($idx, $value)"
      case CXML(idx, value)          => s"($idx, $value)"
      case x@CLongString(idx, _, _)  => s"($idx, LString(${x.longString.size}:${x.longString.makeString.take(32)}..))"
      case x@CLongBytes(idx, _, _)   => s"($idx, LBytes(${x.longBytes.makeBytes.length}))"
    }
    case a: CVIncomparable => a match {
      case CRowID(idx, value)       => s"($idx, RowId($value))"
      case CBFile(idx, value)       => s"($idx, BFile($value))"
      case CNull(idx)               => s"($idx, null)"
      case CNotSupport(idx, jdbcType) => s"($idx, notSupport($jdbcType))"
    }
  }
}

sealed trait CVOrderable extends CVal
sealed trait CVEquatable extends CVal
sealed trait CVIncomparable extends CVal

// --- 1. Orderable ---
final case class CInt(idx: Int, value: Int) extends CVOrderable
final case class CLong(idx: Int, value: Long) extends CVOrderable
final case class CBigInt(idx: Int, value: BigInt) extends CVOrderable
final case class CDouble(idx: Int, value: Double) extends CVOrderable
final case class CDecimal(idx: Int, value: BigDecimal) extends CVOrderable
final case class CString(idx: Int, value: String) extends CVOrderable
final case class CDate(idx: Int, value: LocalDate) extends CVOrderable
final case class CTime(idx: Int, value: LocalTime) extends CVOrderable
final case class CTimestamp(idx: Int, value: LocalDateTime) extends CVOrderable
final case class COffsetTime(idx: Int, value: OffsetTime) extends CVOrderable
final case class COffsetTimestamp(idx: Int, value: OffsetDateTime) extends CVOrderable

// --- 2. Equatable  ---
final case class CBoolean(idx: Int, value: Boolean) extends CVEquatable
final case class CBytes(idx: Int, value: Array[Byte]) extends CVEquatable
final case class CLongString(idx: Int, value: () => LongString, length: Option[Long]) extends CVEquatable
{ lazy val longString: LongString = value() }
final case class CLongBytes(idx: Int, value: () => LongBytes, length: Option[Long]) extends CVEquatable
{ lazy val longBytes: LongBytes = value() }
final case class COraGeometry(idx: Int, value: JGeometry) extends CVEquatable {
  def toWellKnownText: String = COraGeometry.toWkt(value)
  override val valueStr: String = toWellKnownText
}

object COraGeometry {
  val wkbConvert = new WKB()
  val wktConvert = new WKT()
  def toBytes(geo: JGeometry): Array[Byte] = wkbConvert.fromJGeometry(geo)
  def fromBytes(bs: Array[Byte]): JGeometry = wkbConvert.toJGeometry(bs)
  def toWkt(geo:JGeometry) = new String(wktConvert.fromJGeometry(geo), "UTF-8")
}

final case class CInterval(idx: Int, value: String) extends CVEquatable
final case class CXML(idx: Int, value: String) extends CVEquatable

// --- 3. Incomparable (비교 제외 대상) ---
final case class CRowID(idx: Int, value: String) extends CVIncomparable
final case class CBFile(idx: Int, value: String) extends CVIncomparable
final case class CNull(idx: Int) extends CVIncomparable

final case class CNotSupport(idx: Int, jdbcType: Int) extends CVIncomparable

