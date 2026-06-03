package utils

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Try

object LogHelper {
  // logging
  // ================================================================================
  // memo --> debug/trace level
  // note --> info level, sub-flow
  // mark --> warn level, main flow, exception ignorable
  // oops --> err level, fatal error
  // --------------------------------------------------------------------------------
  private def showLog(s: => String) = System.err.println(s)

  def memo(s: => String): Unit = { showLog("\tMemo:\t" + s) }
  def note(s: => String): Unit = { showLog("Note:\t" + s) }
  def mark(s: => String): Unit = { showLog("Mark:\t" + s) }
  def oops(s: => String): Unit = { showLog("Oops:\t" + s) }

  def memo(h: => String, s: => String): Unit = { memo( h + "\n" + s) }
  def note(h: => String, s: => String): Unit = { note( h + "\n" + s) }
  def mark(h: => String, s: => String): Unit = { mark( h + "\n" + s) }
  def oops(h: => String, s: => String): Unit = { oops( h + "\n" + s) }

  def memoWith(s: => String): String = { memo(s); s }
  def noteWith(s: => String): String = { note(s); s }
  def markWith(s: => String): String = { mark(s); s }
  def oopsWith(s: => String): String = { oops(s); s }

  def memoWith[T](s: => String, t: T): T = { memo(s); t }
  def noteWith[T](s: => String, t: T): T = { note(s); t }
  def markWith[T](s: => String, t: T): T = { mark(s); t }
  def oopsWith[T](s: => String, t: T): T = { mark(s); t }
  // --------------------------------------------------------------------------------

  // exception :: --> warn level
  def err(s: String) = new Exception(markWith(s))
  def err(e: Throwable) = { mark(e.toString); e }
  def err(s: String, e: Throwable) = { mark(s + e.toString); e }


  def fault(s: String) = new Exception(oopsWith(s))
  def fault(e: Throwable) = { oops(e.toString); e }
  def fault(s: String, e: Throwable) = { oops(s + e.toString); e }

  // --------------------------------------------------------------------------------
  // time-Log
  // --------------------------------------------------------------------------------

  def timeLog[R](block: => R)(msg: String): R = {
    val t0 = System.nanoTime()
    val ret = block
    val ms = (System.nanoTime() - t0) / 1e6
    note(f"[TimeLog] $msg elapsed: $ms%.2f ms")
    ret
  }

  // string <---> bytes
  // ================================================================================
  @inline def toHex(b: Array[Byte]): String = b.map("%02X".format(_)).mkString
  @inline def bytesToBase64(b: Array[Byte]): String = java.util.Base64.getEncoder.encodeToString(b)
  @inline def base64ToBytes(s: String): Array[Byte] = java.util.Base64.getDecoder.decode(s)

  def extractBetween(s: String, prefix: String, suffix: String): Option[String] = {
    val start = s.indexOf(prefix)
    if (start == -1) return None

    val from = start + prefix.length
    val end = s.indexOf(suffix, from)
    if (end == -1) return None

    Some(s.substring(from, end))
  }

  def getFileList(prefix: String, suffix: String, dir: String = "."): Either[Throwable, List[String]]
  = {
    Try {
      val path = Paths.get(dir)
      Files.list(path)
        .iterator()
        .asScala
        .filter(Files.isRegularFile(_))                // 파일만
        .map(_.getFileName.toString)
        .filter(name => name.startsWith(prefix) && name.endsWith(suffix))
        .toList
    }.toEither
  }

  // lift exception
  // ================================================================================
  def maybe[A](lable: => String)(op: => A): Option[A] = {
    try Option(op) catch {
      case e: Throwable =>
        mark(e.toString)
        None
    }
  }

  def mayNot[A](lable: => String)(op: => A): Option[A] = {
    try Option(op) catch {
      case e: Throwable =>
        oops(e.toString)
        None
    }
  }
}
