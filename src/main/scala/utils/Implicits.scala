package utils

import java.sql.ResultSet

object Implicits {

  implicit class ResultSetIter(rs: ResultSet) {
    def mapIter[A](f: ResultSet => A): Iterator[A] =
      Iterator.continually(rs).takeWhile(_.next()).map(f)
  }

  def emptyIterator[A] = new Iterator[A] with AutoCloseable {
    override def hasNext: Boolean = false
    override def next() = throw new NoSuchElementException("next on empty iterator")
    override def close(): Unit = ()
  }

  implicit class IterWithZip[A] (it: Iterator[A] with AutoCloseable) {
    def zipIndexFrom( start: Int): Iterator[(Int, A)] with AutoCloseable
    = new Iterator[(Int, A)] with AutoCloseable {
      private var idx = start
      override def hasNext: Boolean = it.hasNext
      override def next(): (Int, A) = {
        val a = it.next()
        val ret = (idx, a)
        idx += 1
        ret
      }
      override def close() = it.close()
    }
  }

  implicit class IterWithZip0[A] (it: Iterator[A] ) {
    def zipIndexFrom( start: Int): Iterator[(Int, A)]
    = new Iterator[(Int, A)] {
      private var idx = start
      override def hasNext: Boolean = it.hasNext
      override def next(): (Int, A) = {
        val a = it.next()
        val ret = (idx, a)
        idx += 1
        ret
      }
    }
  }


}
