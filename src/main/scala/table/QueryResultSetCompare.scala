package table

//import schema.TableInfo
//import table.Result._
//import table.comp.CReader
//import utils.LogHelper.{err, mark, timeLog}
//
//import java.sql.{Connection, ResultSet}
//import scala.util.control.NonFatal
//
//case class QueryResultSet(rs: ResultSet,
//                          colShapes: Map[Int, CReader],
//                          onClose: () => Unit ) extends AutoCloseable {
//
//  override def close(): Unit = onClose()
//}
//
//object QueryResultSet {
//  /**
//   * {{{
//   * execute query
//   * caveat: caller MUST CHECK SQL-VALIDATION (sql-injection, or other risk) !!!)
//   * }}}
//   *
//   * @param con       Connection (autoCommit = false)
//   * @param sql       SELECT … ORDER BY …,
//   * @param fetchSize Prefetch 행 수 (500~5,000)
//   * @param tag       tag for desc. (ex: "tableA")
//   *
//   * @note must close() after usage.
//   */
//  def apply(con: Connection,
//            sql: String,
//            fetchSize: Int = 1_000): QueryResultSet = {
//
//    mark("[QueryResultSet] Start query", sql)
//
//    val databaseProductName = con.getMetaData.getDatabaseProductName
//    val statement = con.prepareStatement( sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
//
//    try {
//      statement.setFetchSize(fetchSize)
//
//      // if takes long --> query optimize (maybe sorting effect)
//      val resultSet = timeLog(statement.executeQuery())("executeQuery")
//
//      timeLog(resultSet.isFirst)("executeQuery: first row")
//
//      val colShapes = CReader.getColMetas( resultSet, databaseProductName)
//
//      new QueryResultSet(
//        resultSet,
//        colShapes,
//        () => {
//          resultSet.close()
//          statement.close()
//          con.close()
//        }
//      )
//
//    } catch {
//      case NonFatal(e) =>
//        statement.close()
//        con.close()
//
//        throw err(e)
//    }
//  }
//
//
//  // ================================================================================
//  // caveat :: return Iterator must be used one by one. (don't use buffering)
//  def diffSortedResultSet(qrsA: QueryResultSet,
//                          qrsB: QueryResultSet,
//                          orderBy: RowComp,
//                          equalsOn: RowComp
//                         ): Iterator[Result] with AutoCloseable
//  = new Iterator[Result] with AutoCloseable {
//
//    private val rsA = qrsA.rs
//    private val rsB = qrsB.rs
//
//    private var hasA = false
//    private var hasB = false
//
//    private var nextResult: Option[Result] = None
//
//    private var needMoveA = true    // at first-time, moveCursor is needed.
//    private var needMoveB = true    // at first-time, moveCursor is needed.
//
//    private var currentA: Option[Row] = None
//    private var currentB: Option[Row] = None
//
//    override def close() = {
//      rsA.close()
//      rsB.close()
//    }
//
//    override def hasNext: Boolean = {
//      if (nextResult.isEmpty) {
//        moveCursor()
//        computeNext()
//      }
//      nextResult.nonEmpty
//    }
//
//    override def next(): Result = {
//
//      if (nextResult.isDefined) {
//        val res = nextResult.get
//        nextResult = None // set nextResult to None for marking next calculation.
//        res
//      } else {
//
//        // nextResult is None, so.. computeNext
//        moveCursor()
//        computeNext()
//        nextResult match {
//          case Some(res) => nextResult = None; res
//          case None => close(); throw err( "No more elements")
//        }
//      }
//    }
//
//    // moveCursor
//    @inline private def moveCursor(): Unit = {
//      if (needMoveA) {
//        hasA = rsA.next()
//        if(hasA) currentA = Some(Row(orderBy.rowA(rsA), equalsOn.rowA(rsA))) // read
//        needMoveA = false
//      }
//      if (needMoveB) {
//        hasB = rsB.next()
//        if(hasB) currentB = Some(Row(orderBy.rowB(rsB), equalsOn.rowB(rsB))) // read
//        needMoveB = false
//      }
//    }
//
//    private def consumeA() =  {
//      needMoveA = true
//      Some(OnlyInA(currentA.get))
//    }
//
//    private def consumeB() = {
//      needMoveB = true
//      Some(OnlyInB(currentB.get))
//    }
//
//    private def consumeAB() = {
//      val cmp = orderBy.compare(currentA.get.keys, currentB.get.keys)
//      if (cmp < 0)         consumeA()
//      else if (cmp > 0)    consumeB()
//      else {
//        needMoveA = true
//        needMoveB = true
//        if (equalsOn.equal(currentA.get.cols, currentB.get.cols))
//          Some(Same(currentA.get, currentB.get))
//        else
//          Some(Change(currentA.get, currentB.get))
//      }
//    }
//
//    private def computeNext(): Unit = {
//
//      nextResult = (hasA, hasB) match {
//        case (false, false) => close(); None
//        case (false, true) =>  consumeB()
//        case (true, false) =>  consumeA()
//        case (true, true)  =>  consumeAB()
//      }
//    }
//  }
//
//
//
////  def diffIt(conA: Connection, conB: Connection,
////             tableStructure: TableInfo): Unit = {
////
////
////    val cols = tableStructure.cols.toList
////
////
////  }
//}
