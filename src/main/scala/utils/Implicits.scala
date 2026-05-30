package utils

import java.sql.ResultSet

object Implicits {

  implicit class ResultSetIter(rs: ResultSet) {
    def mapIter[A](f: ResultSet => A): Iterator[A] =
      Iterator.continually(rs).takeWhile(_.next()).map(f)
  }


}
