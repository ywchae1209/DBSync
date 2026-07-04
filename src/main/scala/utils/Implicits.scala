package utils

import java.sql.ResultSet

object Implicits {

  implicit class ResultSetIter(rs: ResultSet) {
    def mapIter[A](f: ResultSet => A): Iterator[A] =
      Iterator.continually(rs).takeWhile(_.next()).map(f)
  }

  implicit class IterWithZip[A] (it: Iterator[A]) {
    def zipIndexFrom( start: Int = 1): Iterator[(Int, A)]
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
