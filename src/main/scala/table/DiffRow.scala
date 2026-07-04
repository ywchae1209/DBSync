package table

import tui.ReportMsgStop
import tui.TaskStatus._
import tui.layoutzEx.StringWithColor

import scala.util.Try

sealed trait DiffRow {

  def isInsert = DiffRow.isOnlyA(this)
  def isUpdate = DiffRow.isUpdate(this)
  def isDelete = DiffRow.isOnlyB(this)
  def isSame   = DiffRow.isSame(this)

  def keys: List[CVal]
  def serialize = DiffRowSerDe.serialize(this)
  def toPretty: String = {
    val oaStr = "Insert: ".yellow
    val obStr = "Delete: ".yellow
    val upStr = "Update: ".yellow
    val saStr = "Same ".yellow
    val kstr = "key".green + "["
    val cstr = "col".green + "["
    val estr = "]"

    this match {
      case OnlyInA(keys, sourceVals) =>
        oaStr +
          (if (keys.nonEmpty) keys.mkString(kstr, ", ", estr) else "") +
          (if (sourceVals.nonEmpty) keys.mkString(cstr, ", ", estr) else "")
      case OnlyInB(keys) =>
        obStr +
          (if (keys.nonEmpty) keys.mkString(kstr, ", ", estr) else "")
      case Update(keys, sourceVals) =>
        upStr +
          (if (keys.nonEmpty) keys.mkString(kstr, ", ", estr) else "") +
          (if (sourceVals.nonEmpty) keys.mkString(cstr, ", ", estr) else "")
      case Same(keys) =>
        saStr + (if (keys.nonEmpty) keys.mkString(kstr, ", ", estr) else "")
    }
  }
}
object DiffRow {
  def fromBytes(bs: Array[Byte]) = DiffRowSerDe.deserialize(bs)

  val isOnlyA = (dr: DiffRow) => dr.isInstanceOf[OnlyInA]
  val isOnlyB = (dr: DiffRow) => dr.isInstanceOf[OnlyInB]
  val isUpdate = (dr: DiffRow) => dr.isInstanceOf[Update]
  val isSame = (dr: DiffRow) => dr.isInstanceOf[Same]
  val isNotSame =(dr: DiffRow) => !dr.isInstanceOf[Same]
}

final case class OnlyInA(keys: List[CVal], sourceVals: List[CVal]) extends DiffRow {
  override def toString: String = keys.mkString("A (@:[", ", ", "],") + sourceVals.mkString("#:[", ", ", "]")
}
final case class OnlyInB(keys: List[CVal] ) extends DiffRow {
  override def toString: String = keys.mkString("B (@:[", ", ", "])")

}

final case class Update(keys: List[CVal], sourceVals: List[CVal] ) extends DiffRow {
  override def toString: String = keys.mkString("U (@:[", ", ", "],") + sourceVals.mkString("A:[", ", ", "]")

}

final case class Same(keys: List[CVal] ) extends DiffRow {
  override def toString: String = keys.mkString("S (@:[", ", ", "])")
}

import oracle.spatial.util.WKB
import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker}
import table.LongCollection.LongBytes.fromBytes
import table.LongCollection.LongString.fromString
import tui.ReportMsg

import java.io.ByteArrayOutputStream

object DiffRowSerDe {

  import java.io.{FileInputStream, FileOutputStream}
  import scala.util.Using

  def readDiffRows( path: String ) = {
    Try {
      val fis = new FileInputStream(path)
      val unpacker: MessageUnpacker = MessagePack.newDefaultUnpacker(fis)

      new Iterator[DiffRow] with AutoCloseable {

        private var nextRow: Option[DiffRow] = fetchNext()
        private def fetchNext(): Option[DiffRow] = {
          if (unpacker.hasNext) {
            val out = unpackDiffRow(unpacker)
            Some(out)
          } else {
            fis.close()
            None
          }
        }
        override def hasNext: Boolean = nextRow.isDefined
        override def next(): DiffRow = {
          val current = nextRow.get
          nextRow = fetchNext()
          current
        }
        override def close(): Unit = fis.close()
      }
    }.toEither
  }

  def writeDiffRows(name: String, path: String, rows: Iterator[DiffRow],
                    cancel: () => Boolean, callback: ReportMsg => Unit)
  : Either[Throwable, Long] = {
    Using(new FileOutputStream(path)) { fos =>
      val packer: MessagePacker = MessagePack.newDefaultPacker(fos)
      try {
        var total = 0L
        rows.foreach { row =>
          if(cancel()) {
            packer.flush()
            callback( ReportMsgStop(name, s"[Write Stopped] $path (written: $total) by user") )
            return Right(total)
          }
          total = total + 1
          packDiffRow(row, packer)
          if(total % 512 == 0) packer.flush()
        }
        packer.flush()
        total
      } finally {
        packer.close()
      }
    }.toEither
  }

  //
  private object TypeID {
    val INT = 1; val LONG = 2; val BIGINT = 3; val DOUBLE = 4; val DECIMAL = 5
    val STRING = 6; val DATE = 7; val TIME = 8; val TIMESTAMP = 9
    val OFFSET_TIME = 10; val OFFSET_TIMESTAMP = 11; val BOOLEAN = 12; val BYTES = 13
    val GEOMETRY = 14; val INTERVAL = 15; val XML = 16; val LONG_STRING = 17
    val LONG_BYTES = 18; val ROWID = 19; val BFILE = 20; val NULL = 21; val NOT_SUPPORT = 22
  }

  //
  private object DiffTypeID {
    val ONLY_IN_A = 101
    val ONLY_IN_B = 102
    val UPDATE    = 103
    val SAME      = 104
  }

  //  SERIALIZATION
  // ==========================================

  def serialize(row: DiffRow): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val packer = MessagePack.newDefaultPacker(baos)
    try {
      packDiffRow(row, packer)
      packer.flush()
      baos.toByteArray
    } finally {
      packer.close()
    }
  }

  private def packDiffRow(row: DiffRow, packer: MessagePacker): Unit = {
    row match {
      case OnlyInA(keys, sourceVals) =>
        packer.packInt(DiffTypeID.ONLY_IN_A)
        packCValList(keys, packer)
        packCValList(sourceVals, packer)

      case OnlyInB(keys) =>
        packer.packInt(DiffTypeID.ONLY_IN_B)
        packCValList(keys, packer)

      case Update(keys, sourceVals) =>
        packer.packInt(DiffTypeID.UPDATE)
        packCValList(keys, packer)
        packCValList(sourceVals, packer)

      case Same(keys) =>
        packer.packInt(DiffTypeID.SAME)
        packCValList(keys, packer)
    }
  }

  private def packCValList(list: List[CVal], packer: MessagePacker): Unit = {
    packer.packArrayHeader(list.size)
    list.foreach(cval => packCVal(cval, packer))
  }

  val packZoneOffset = java.time.ZoneOffset.UTC
  val wkbConverter = new WKB()

  private def packCVal(cval: CVal, p: MessagePacker): Unit = {

    // index
    p.packInt(cval.idx)

    cval match {
      case CInt(_, v)       => p.packInt(TypeID.INT); p.packInt(v)
      case CLong(_, v)      => p.packInt(TypeID.LONG); p.packLong(v)
      case CBigInt(_, v)    => p.packInt(TypeID.BIGINT); p.packString(v.toString)   // BigInt => String
      case CDouble(_, v)    => p.packInt(TypeID.DOUBLE); p.packDouble(v)
      case CDecimal(_, v)   => p.packInt(TypeID.DECIMAL); p.packString(v.toString) // BigDecimal => String
      case CString(_, v)    => p.packInt(TypeID.STRING); p.packString(v)
      case CDate(_, v)      => p.packInt(TypeID.DATE); p.packLong(v.toEpochDay)
      case CTime(_, v)      => p.packInt(TypeID.TIME); p.packLong(v.toNanoOfDay)

      case CTimestamp(_, v) => p.packInt(TypeID.TIMESTAMP)
        p.packLong(v.toEpochSecond(packZoneOffset))
        p.packInt(v.getNano)

      case COffsetTime(_, v)     => p.packInt(TypeID.OFFSET_TIME)
        p.packLong(v.toLocalTime.toNanoOfDay)   // 하루 중 나노초 (Long)
        p.packInt(v.getOffset.getTotalSeconds)  // 오프셋 시차 초 (Int, 예: +09:00 = 32400)

      case COffsetTimestamp(_, v)=> p.packInt(TypeID.OFFSET_TIMESTAMP);
        p.packLong(v.toEpochSecond)            // 에포크 초 (Long)
        p.packInt(v.getNano)                   // 나노초 잔여물 (Int)
        p.packInt(v.getOffset.getTotalSeconds) // 오프셋 시차 초 (Int)

      case CBoolean(_, v)   => p.packInt(TypeID.BOOLEAN); p.packBoolean(v)

      case CBytes(_, v)     => p.packInt(TypeID.BYTES)
        p.packBinaryHeader(v.length)
        p.addPayload(v)

      case x: COraGeometry  => p.packInt(TypeID.GEOMETRY)
        val bs = COraGeometry.toBytes(x.value)    // Array[Byte]
        p.packBinaryHeader(bs.length)
        p.addPayload(bs)

      case CInterval(_, v)  => p.packInt(TypeID.INTERVAL); p.packString(v)
      case CXML(_, v)       => p.packInt(TypeID.XML); p.packString(v)

      // LONG types
      case x: CLongString   => p.packInt(TypeID.LONG_STRING); p.packString(x.longString.makeString)
      case x: CLongBytes    => p.packInt(TypeID.LONG_BYTES)
        p.packBinaryHeader(x.longBytes.size)
        p.addPayload(x.longBytes.makeBytes)

      case CRowID(_, v)     => p.packInt(TypeID.ROWID); p.packString(v)
      case CBFile(_, v)     => p.packInt(TypeID.BFILE); p.packString(v)
      case CNull(_)         => p.packInt(TypeID.NULL)

      case CNotSupport(_, j)=> p.packInt(TypeID.NOT_SUPPORT); p.packInt(j)
    }
  }

  //  DESERIALIZATION
  // ==========================================
  def deserialize(bytes: Array[Byte]): DiffRow = {
    val unpacker = MessagePack.newDefaultUnpacker(bytes)
    try {
      unpackDiffRow(unpacker)
    } finally {
      unpacker.close()
    }
  }

  private def unpackDiffRow(unpacker: MessageUnpacker): DiffRow = {
    val diffRowType = unpacker.unpackInt()
    diffRowType match {
      case DiffTypeID.ONLY_IN_A =>
        val keys = unpackCValList(unpacker)
        val sourceVals = unpackCValList(unpacker)
        OnlyInA(keys, sourceVals)

      case DiffTypeID.ONLY_IN_B =>
        val keys = unpackCValList(unpacker)
        OnlyInB(keys)

      case DiffTypeID.UPDATE =>
        val keys = unpackCValList(unpacker)
        val sourceVals = unpackCValList(unpacker)
        Update(keys, sourceVals)

      case DiffTypeID.SAME =>
        val keys = unpackCValList(unpacker)
        Same(keys)

      case unknown =>
        throw new IllegalArgumentException(s"unknown type ID: $unknown")
    }
  }

  private def unpackCValList(unpacker: MessageUnpacker): List[CVal] = {
    val size = unpacker.unpackArrayHeader()
    val builder = List.newBuilder[CVal]
    for (_ <- 0 until size) {
      builder += unpackCVal(unpacker)
    }
    builder.result()
  }

  import org.msgpack.core.MessageUnpacker

  import java.math.BigDecimal


  private def unpackCVal(unpacker: MessageUnpacker): CVal = {

    val idx = unpacker.unpackInt()
    val typeId = unpacker.unpackInt()

    // 2. TypeID 기반 고속 이진 복원 매칭
    typeId match {
      case TypeID.INT       => CInt(idx, unpacker.unpackInt())
      case TypeID.LONG      => CLong(idx, unpacker.unpackLong())
      case TypeID.BIGINT    => CBigInt(idx, BigInt(unpacker.unpackString())) // 정밀도 유지를 위해 String 기반 복원
      case TypeID.DOUBLE    => CDouble(idx, unpacker.unpackDouble())
      case TypeID.DECIMAL   => CDecimal(idx, new BigDecimal(unpacker.unpackString()))
      case TypeID.STRING    => CString(idx, unpacker.unpackString())
      case TypeID.DATE      => CDate(idx, java.time.LocalDate.ofEpochDay(unpacker.unpackLong()))
      case TypeID.TIME      => CTime(idx, java.time.LocalTime.ofNanoOfDay(unpacker.unpackLong()))

      case TypeID.TIMESTAMP =>
        val epochSec = unpacker.unpackLong()
        val nanoAdj = unpacker.unpackInt()
        CTimestamp(idx, java.time.LocalDateTime.ofEpochSecond(epochSec, nanoAdj, packZoneOffset))

      case TypeID.OFFSET_TIME =>
        val nanos = unpacker.unpackLong()
        val offsetSec = unpacker.unpackInt()
        val localTime = java.time.LocalTime.ofNanoOfDay(nanos)
        val targetOffset = java.time.ZoneOffset.ofTotalSeconds(offsetSec)
        COffsetTime(idx, java.time.OffsetTime.of(localTime, targetOffset))

      case TypeID.OFFSET_TIMESTAMP =>
        val epochSec = unpacker.unpackLong()
        val nanoAdj = unpacker.unpackInt()
        val offsetSec = unpacker.unpackInt()
        val targetOffset = java.time.ZoneOffset.ofTotalSeconds(offsetSec)
        COffsetTimestamp(idx, java.time.OffsetDateTime.ofInstant(
          java.time.Instant.ofEpochSecond(epochSec, nanoAdj),
          targetOffset
        ))

      case TypeID.BOOLEAN   => CBoolean(idx, unpacker.unpackBoolean())

      case TypeID.BYTES     =>
        val len = unpacker.unpackBinaryHeader()
        val bytes = new Array[Byte](len)
        unpacker.readPayload(bytes)
        CBytes(idx, bytes)

      case TypeID.GEOMETRY  =>
        val len = unpacker.unpackBinaryHeader()
        val bytes = new Array[Byte](len)
        unpacker.readPayload(bytes)
        val j = COraGeometry.fromBytes(bytes)
        COraGeometry(idx, j)
      case TypeID.INTERVAL  => CInterval(idx, unpacker.unpackString())
      case TypeID.XML       => CXML(idx, unpacker.unpackString())

      case TypeID.LONG_STRING =>
        val s = unpacker.unpackString()
        CLongString(idx, () => fromString(s), None)

      case TypeID.LONG_BYTES =>
        val l = unpacker.unpackBinaryHeader()
        val bs = new Array[Byte](l)
        unpacker.readPayload(bs)
        CLongBytes(idx, () => fromBytes(bs), None)

      case TypeID.ROWID        => CRowID(idx, unpacker.unpackString())
      case TypeID.BFILE        => CBFile(idx, unpacker.unpackString())
      case TypeID.NULL         => CNull(idx)

      case TypeID.NOT_SUPPORT  => CNotSupport(idx, unpacker.unpackInt())

      case unknown =>
        throw new IllegalStateException(s"MessagePack corrupted TypeID, or value of jdbc.type = $unknown")
    }
  }
}