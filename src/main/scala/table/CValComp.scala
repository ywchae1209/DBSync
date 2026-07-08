package table

import oracle.spatial.geometry.JGeometry
import utils.LogHelper.{fault, memo}
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time._
import java.util

sealed trait Tolerance {

  val asMilli: Long = this match {
    case DeltaMillis(milli) => milli.toLong
    case DeltaDouble(delta) => (delta * 1000.0).toLong
  }

  val asDouble: Double = this match {
    case DeltaMillis(milli) => milli * 0.001
    case DeltaDouble(delta) => delta
  }

  val asDecimal: BigDecimal = BigDecimal.valueOf(asDouble)
}

object Tolerance {
  implicit val jsonCodec: JsonCodec[Tolerance] = DeriveJsonCodec.gen[Tolerance]
  implicit val jsonCodec1: JsonCodec[DeltaDouble] = DeriveJsonCodec.gen[DeltaDouble]
  implicit val jsonCodec2: JsonCodec[DeltaMillis] = DeriveJsonCodec.gen[DeltaMillis]


}

case class DeltaMillis(milli: Int) extends Tolerance
case class DeltaDouble(delta: Double) extends Tolerance

// ================================================================================

case class CValComp(colA: CReader,
                    colB: CReader,
                    ascending: Boolean = true,          // in oracle : ascending is default.
                    nullAsSmallest: Boolean = false,    // in oracle : null is biggest-one.
                    tolerance: Option[Tolerance] = None,
                    equalCheckOnly: Boolean = false)
{

  def compare(a: CVal, b: CVal): Int = {
    val out = (a, b) match {
      case (_: CNull, _: CNull)               => 0
      case (_: CNull, _)                      => if (nullAsSmallest) -1 else 1
      case (_, _: CNull)                      => if (nullAsSmallest) 1 else -1
      case (av: CVOrderable, bv: CVOrderable) => isAscending(av, bv)
      case (av: CVEquatable, bv: CVEquatable) => if (CValComp.isEqual(av, bv)) 0 else -1 // if not-equal, treat as asc.
      case (av: CVIncomparable, bv: CVIncomparable) => if(CValComp.isEqual(av, bv)) 0 else -1
      case _ =>
        throw fault(s"Incomparable types: $a vs $b")
    }
    if (ascending) out else -out
  }

  def equal(a: CVal, b: CVal): Boolean = (a, b) match {
    case (_: CNull, _: CNull)               => true
    case (_: CNull, _) | (_, _: CNull)      => false
    case (av: CVEquatable, bv: CVEquatable) => CValComp.isEqual(av, bv)
    case (av: CVOrderable, bv: CVOrderable) => isAscending(av, bv) == 0
    case _ => throw fault(s"not-equitable types: $a vs $b")
  }

  def isAscending(a: CVOrderable, b: CVOrderable): Int = (a, b) match {
    case (CInt(_, x), CInt(_, y))             => x.compare(y)
    case (CLong(_, x), CLong(_, y))           => x.compare(y)
    case (CBigInt(_, x), CBigInt(_, y))       => x.compareTo(y)
    case (CDouble(_, x), CDouble(_, y))       => CValComp.compareDouble(x, y, tolerance)
    case (CDecimal(_, x), CDecimal(_, y))     => CValComp.compareBigDecimal(x, y, tolerance)
    case (CString(_, x), CString(_, y))       => x.compareTo(y)
    case (CDate(_, x), CDate(_, y))           => x.compareTo(y)
    case (CTime(_, x), CTime(_, y))           => x.compareTo(y)
    case (CTimestamp(_, x), CTimestamp(_, y)) => CValComp.compareTimestamp(x, y, tolerance)
    case (COffsetTime(_, x),COffsetTime(_, y))=> CValComp.compareOffsetTime(x, y, tolerance)
    case (COffsetTimestamp(_, x), COffsetTimestamp(_, y)) => CValComp.compareOffsetDateTime(x, y, tolerance)
    case _ => throw fault(s"Not same-type-columns")
  }

}

object CValComp {

  // 기본 tolerance
  // ================================================================================
  var doubleTolerance = Some(DeltaDouble(1e-7))
  var timeTolerance = Some(DeltaMillis(1))
  // 1e-15  double precision 한계
  // 1e-12  JVM/ojdbc 흔한 오차
  // 1e-9   generous 오차 (1000배 여유)
  // 1e-6   GIS meter 수준 (≈ mm~cm)
  // --------------------------------------------------------------------------------
  var geoAbsTolerance = DeltaDouble(1e-9)
  var geoRelTolerance = DeltaDouble(1e-9)
  // ================================================================================

  def isEqual(a: CVEquatable, b: CVEquatable): Boolean = (a, b) match {
    case (CBoolean(_, x), CBoolean(_, y))             => x == y
    case (CBytes(_, x), CBytes(_, y))                 => util.Arrays.equals(x, y)
    case (a:CLongString, b:CLongString)               => a.longString.compare(b.longString) == 0 // memo("comparing : LongString")
    case (a:CLongBytes, b:CLongBytes)                 => a.longBytes.compare(b.longBytes) == 0 // memo("comparing : LongBytes")
    case (COraGeometry(_, x), COraGeometry(_, y))     => fastEquals(x, y)// nestedEquals(x, y)
    case (CInterval(_, x), CInterval(_, y))           => x == y
    case (CXML(_, x), CXML(_, y))                     => xmlEquals(x, y)
    case _ => throw fault(s"isEqual: Not unsupported-type-columns")
  }

  def isEqual(a: CVIncomparable, b: CVIncomparable): Boolean = (a, b) match {
    case (CRowID(_, x), CRowID(_, y))     => x == y
    case (CBFile(_, x), CBFile(_, y))     => x == y
    case (CNotSupport(i, x), _)           => throw fault(s"isEqual: Not-Support type : idx = $i")
    case (_, CNotSupport(i, x))           => throw fault(s"isEqual: Not-Support type : idx = $i")
    case _                                => throw fault(s"isEqual: incomparable")

  }

  def xmlEquals(a: String, b: String): Boolean = {

    def normalizeXml(s: String): String =
      Option(s)
        .map(_.replaceAll(">\\s+<", "><") // 태그 사이 공백 제거
          .replaceAll("\\s+$", "")        // trailing newline 제거
          .replaceAll("\r", "")           // CR 제거 (환경 차이)
          .trim)
        .orNull

    val na = normalizeXml(a)
    val nb = normalizeXml(b)
    na == nb
  }
  import oracle.spatial.geometry.JGeometry



  def fastEquals( a: JGeometry,
                  b: JGeometry,
                  absTol: Double = geoAbsTolerance.asDouble,
                  relTol: Double = geoRelTolerance.asDouble): Boolean = {

    if (a eq b) return true
    if (a == null || b == null) return false

    if (a.getType != b.getType) return false
    if (a.getSRID != b.getSRID) return false

    val aElem = a.getElemInfo
    val bElem = b.getElemInfo
    if (!java.util.Arrays.equals(aElem, bElem)) return false

    val aOrd = a.getOrdinatesArray
    val bOrd = b.getOrdinatesArray

    if (aOrd == null || bOrd == null) return aOrd == bOrd
    if (aOrd.length != bOrd.length) return false

    var i = 0
    while (i < aOrd.length) {
      val x = aOrd(i)
      val y = bOrd(i)

//      if (x != y) {
        val diff = Math.abs(x - y)
        val tol = Math.max(absTol, relTol * Math.max(Math.abs(x), Math.abs(y)))
        if (diff > tol) return false
//      }
      i += 1
    }
    true
  }

  def fastExactEquals(a: JGeometry, b: JGeometry): Boolean = {
    if (a eq b) return true
    if (a == null || b == null) return false

    if (a.getType != b.getType) return false
    if (a.getSRID != b.getSRID) return false

    val aElem = a.getElemInfo
    val bElem = b.getElemInfo
    if (!java.util.Arrays.equals(aElem, bElem)) return false

    val aOrd = a.getOrdinatesArray
    val bOrd = b.getOrdinatesArray
    if (!java.util.Arrays.equals(aOrd, bOrd)) return false

    true
  }

  // nested struct compare
  def nestedEquals(a: Any, b: Any) : Boolean = {

    val ret = (a, b) match {
      case (null, null) => true
      case (null, _) | (_, null) => false

      case (s1: java.sql.Struct, s2: java.sql.Struct) => nestedEquals(s1.getAttributes, s2.getAttributes)
      case (arr1: java.sql.Array, arr2: java.sql.Array) => nestedEquals(arr1.getArray, arr2.getArray)
      case (a1: Array[_], a2: Array[_]) => a1.length == a2.length && a1.zip(a2).forall { case (x, y) => nestedEquals(x, y) }
      case (x, y) => x == y
    }
    ret
  }

  @inline private def compareWithTolerance[T](a: T, b: T, tolerance: T)(implicit num: Numeric[T], ord: Ordering[T]): Int = {
    val diff = num.minus(a, b)
    val absDiff = num.abs(diff)
    if (ord.lteq(absDiff, tolerance)) 0
    else if (ord.lt(diff, num.zero)) -1 else 1
  }

  def compareDouble(a: Double, b: Double, tol: Option[Tolerance]): Int = tol match {
    case None => a.compare(b)
    case Some(t) => compareWithTolerance(a, b, t.asDouble)
  }

  def compareBigDecimal(a: BigDecimal, b: BigDecimal, tol: Option[Tolerance]): Int = tol match {
    case None => a.compareTo(b)
    case Some(t) => compareWithTolerance(a, b, t.asDecimal)
  }

  def compareTimestamp(a: LocalDateTime, b: LocalDateTime, tol: Option[Tolerance]): Int = tol match {
    case None => a.compareTo(b)
    case Some(t) =>
      val diff = Duration.between(b, a).toMillis // (a - b)
      if (Math.abs(diff) <= t.asMilli) 0 else diff.sign.toInt
  }

  def compareOffsetTime(a: OffsetTime, b: OffsetTime, tol: Option[Tolerance]): Int = tol match {
    case None => a.compareTo(b)
    case Some(t) =>
      val diff = Duration.between(b, a).toMillis
      if (Math.abs(diff) <= t.asMilli) 0 else diff.sign.toInt
  }

  def compareOffsetDateTime(a: OffsetDateTime, b: OffsetDateTime, tol: Option[Tolerance]): Int = tol match {
    case None => a.compareTo(b)
    case Some(t) =>
      val diff = Duration.between(b, a).toMillis
      if (Math.abs(diff) <= t.asMilli) 0 else diff.sign.toInt
  }
}