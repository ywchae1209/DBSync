package table

import utils.LogHelper.{err, mark, memo, oops}

import java.io.{ByteArrayInputStream, Reader, SequenceInputStream, StringReader}
import scala.jdk.CollectionConverters.IteratorHasAsJava

object LongCollection {

  import java.io.InputStream
  import scala.annotation.tailrec
  import scala.util.Using

  /**
   * - chunkDataA, chunkDataB: LazyList[C] (C는 인덱스로 접근 가능한 타입)
   * - get: chunk에서 인덱스로 값을 얻는 함수
   * - length: chunk에서 길이를 얻는 함수
   * - 반환: 사전식 비교 결과 (음수, 0, 양수)
   */
  private def compareChunks[C](aChunks: Seq[C], bChunks: Seq[C])
                              (get: (C, Int) => Int, length: C => Int): Int = {
    @annotation.tailrec
    def compareSameLength(aChunk: C, bChunk: C, ai: Int, bi: Int, len: Int): Int = {
      if (ai >= len && bi >= len) 0
      else {
        val ac = get(aChunk, ai)
        val bc = get(bChunk, bi)
        if (ac != bc) ac - bc
        else compareSameLength(aChunk, bChunk, ai + 1, bi + 1, len)
      }
    }

    @annotation.tailrec
    def loop(aL: Seq[C], bL: Seq[C], ai: Int, bi: Int): Int = {
      (aL.headOption, bL.headOption) match {
        case (None, None)    => 0
        case (None, Some(_)) => -1
        case (Some(_), None) => 1
        case (Some(aChunk), Some(bChunk)) =>
          val aLen = length(aChunk)
          val bLen = length(bChunk)

          if(aLen == bLen) {
            val cmp = compareSameLength(aChunk, bChunk, ai, bi, aLen)
            if (cmp != 0) cmp
            else loop(aL.tail, bL.tail, 0, 0)
          }

          else if (ai >= aLen) loop(aL.tail, bL, 0, bi)
          else if (bi >= bLen) loop(aL, bL.tail, ai, 0)
          else {
            val ac = get(aChunk, ai)
            val bc = get(bChunk, bi)
            if (ac != bc) ac - bc
            else loop(aL, bL, ai + 1, bi + 1)
          }
      }
    }
    val ret = loop(aChunks, bChunks, 0, 0)

    ret

  }

  // ================================================================================
  case class LongLazyBytes(chunks: LazyList[Array[Byte]])
  {
    def compare(other: LongLazyBytes): Int = {
      compareChunks(chunks, other.chunks)(
        get = (arr, idx) => arr(idx) & 0xFF,
        length = _.length
      )
    }
    def iterator: Iterator[Array[Byte]] = chunks.iterator
  }

  object LongLazyBytes {

    def toInputStream(iter : Iterator[Array[Byte]], tag: String = ""): InputStream = {
      val streams = iter.map { case (bytes) => new ByteArrayInputStream(bytes) }.asJavaEnumeration
      val str = if(tag.isEmpty) "LongLazyBytes" else tag
      new SequenceInputStream(streams){
        override def toString: String = str
      }
    }

    def fromInputStream(is: InputStream, chunkSize: Int): LazyList[Array[Byte]] = {

      def readNext(): Option[(Array[Byte], Unit)] =
        try {
          val buf = new Array[Byte](chunkSize)
          val bytesRead = is.read(buf)
          if (bytesRead == -1) {
            is.close() // EOF → close
            None
          } else if (bytesRead < chunkSize) {
            // last chunk: 실제 읽은 크기만큼만 slice
            Some(buf.take(bytesRead) -> ())
          } else {
            Some(buf -> ())
          }
        } catch {
          case e: Throwable =>
            try is.close() catch { case e: Throwable => mark(e.toString)}
            throw err(e)
        }

      LazyList.unfold(())(_ => readNext())
    }
  }

  // ================================================================================
  case class LongLazyString(chunks: LazyList[String])
  {
    def compare(other: LongLazyString): Int
    = {
      compareChunks(chunks, other.chunks)(
        get = (str, idx) => str.charAt(idx),
        length = _.length
      )
    }
    def iterator = chunks.iterator

  }

  object LongLazyString {

    def toReader(iter: Iterator[String], tag: String = ""): Reader
    = new Reader {

      override def toString: String = if(tag.isEmpty)"LongLazyString" else tag
      private var current = if (iter.hasNext) new StringReader(iter.next()) else null

      override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
        while (current != null) {
          val n = current.read(cbuf, off, len)
          if (n != -1) return n
          current.close()
          current = if (iter.hasNext) new StringReader(iter.next()) else null
        }
        -1
      }

      override def close(): Unit = {
        if (current != null) {
          current.close()
          current = null
        }
      }
    }

    def fromReader(reader: java.io.Reader, chunkSize: Int): LazyList[String] = {

      def readNext(): Option[(String, Unit)] =
        try {
          val buf = new Array[Char](chunkSize)
          val readCount = reader.read(buf)
          if (readCount == -1) {
            reader.close() // EOF → close
            None
          } else {
            Some(new String(buf, 0, readCount) -> ())
          }
        } catch {
          case e: Throwable =>
            try reader.close() catch { case e: Throwable => err(e) }
            throw err(e)
        }

      LazyList.unfold(())(_ => readNext())
    }
  }


  // ================================================================================
  final case class LongBytes (var chunkData: Vector[(Array[Byte], Int)] = Vector.empty)
  {

    val chunkCount: Int = chunkData.length

    def chunks = chunkData.map(_._1)
    def size: Int = chunkData.map(_._2).sum

    def makeBytes = {
      val len = size
      val result = new Array[Byte](len)

      // local mutation for performance
      var destPos = 0
      var i = 0
      while (i < chunkCount) {
        val (src, srcLen) = chunkData(i)
        System.arraycopy(src, 0, result, destPos, srcLen)
        destPos += src.length
        i += 1
      }
      result
    }

    def append(chunk: Array[Byte]): Unit = {
      val len = chunk.length
      chunkData :+= (chunk -> len)
    }

    def clear(): Unit = {
      chunkData = Vector.empty
    }

    def toInputStream: InputStream = {
      val streams = chunkData.iterator.map { case (bytes, _) => new ByteArrayInputStream(bytes) }.asJavaEnumeration
      new SequenceInputStream(streams)
    }

    /** lexicographical compare */
    def compare(other: LongBytes): Int = {
      if (this eq other) return 0

      compareChunks(chunkData.map(_._1), other.chunkData.map(_._1))(
        get = (arr, idx) => arr(idx) & 0xFF,
        length = _.length
      )
    }
  }

  object LongBytes {

    def fromBytes(s: Array[Byte]) = LongBytes(Vector(s -> s.length ))

    private val DefaultChunkSize = 1024 * 1024 // 1MB : Chunk Bytes

    def fromInputStream(is: () => InputStream, chunkSize: Int = DefaultChunkSize): () => LongBytes = {
      () =>
      try {
        Using.resource(is()) { stream =>

          @tailrec
          def readAll(acc: Vector[(Array[Byte], Int)] = Vector.empty): Vector[(Array[Byte], Int)] = {
            val buf = new Array[Byte](chunkSize)
            val read = stream.read(buf)
            if (read == -1) acc
            else readAll(acc :+ (buf.take(read), read))
          }
          LongBytes(readAll())
        }
      } catch {
        case e: java.io.IOException =>
          oops("[LongBytes: fromInputStream] " + e.toString)
          throw e
      }
    }
  }

  // ================================================================================
  final case class LongString (private var chunkData: Vector[(String, Int)] = Vector.empty)
  {
    def chunks = chunkData.map(_._1)

    def makeString =  chunkData.map(_._1).mkString

    val chunkCount: Int = chunkData.length

    def size:Long = chunkData.map(_._2).sum

    def append(chunk: String): Unit = {
      memo( s"[LongString] append $chunk")
      val len = chunk.length
      chunkData :+= (chunk -> len)
    }

    def clear(): Unit = {
      chunkData = Vector.empty
    }

    def toReader = LongLazyString.toReader( chunkData.iterator.map(_._1))

    def compare(other: LongString): Int = {
      if (this eq other) return 0

      compareChunks(chunkData.map(_._1), other.chunkData.map(_._1))(
        get = (str, idx) => str.charAt(idx),
        length = s =>  {
          s.length
        }
      )
    }
  }

  object LongString {

    def fromString(s: String) = LongString(Vector(s -> s.length ))

    private val DefaultChunkSize = 1024 * 1024 // Chunk String Length

    def fromReader(reader: () => java.io.Reader, chunkSize: Int = DefaultChunkSize)
    : () => LongString = () =>
      try {
        Using.resource(reader()) { r =>
          @tailrec
          def readAll(acc: Vector[(String, Int)] = Vector.empty): Vector[(String, Int)] = {
            val buf = new Array[Char](chunkSize)
            val read = r.read(buf)
            if (read == -1) acc
            else readAll(acc :+ (new String(buf, 0, read), read))
          }
          LongString(readAll())
        }
      } catch {
        case e: java.io.IOException =>
          oops("[LongString:fromReader]" + e.toString)
          throw e
      }
  }
}
