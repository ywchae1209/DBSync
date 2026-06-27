package schema

import com.zaxxer.hikari.HikariDataSource
import tui.SyncTUI.bullet
import tui.layoutzEx._
import zio.json.{DeriveJsonCodec, JsonCodec}

case class DBConf(url: String, user: String, schema: String, pass: Option[String] = None) {

  private var ds: Option[HikariDataSource] = None

  def withoutPass = copy(pass = None)

  def initDataSource(kind:String, callback: String => Unit): Boolean
  = {
    close()
    val out = pass
      .orElse{callback(bullet + s"pass is not set($kind)."); None}
      .flatMap( p => DBConf.createHikariDataSource(url, user,p, callback) )

    ds = out
    ds.isDefined
  }

  def dataSourceOr(kind: String, callback: String => Unit) = {
    if(ds.isEmpty) callback( bullet + s"connection pool not initialized($kind).")
    ds
  }

  def display(kind: String, callback: String => Unit): Unit = {
    callback( layout( "",
      s"-- connection setting for " + kind.color(Color.Yellow).render + " ---",
      table(
        Seq( "url", "schema", "id", "pwd"),
        Seq( Seq( url, schema, user , pass.mkString.map(_ => '*') ) ) ) ).render )
  }

  def alreadyInitalized(kind: String, callback: String => Unit) = {
    if(ds.nonEmpty) callback( bullet + s"connection pool already exists($kind).")
    ds.nonEmpty
  }

  def passIsNotSet(kind: String, callback: String => Unit) = {
    pass match {
      case Some(_) => true
      case None    => callback(bullet + s"password is not set($kind).")
        false
    }
  }

  def close() = ds.foreach(_.close())

}

object DBConf {

  implicit val jsonCodec: JsonCodec[DBConf] = DeriveJsonCodec.gen[DBConf]

  import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

  def apply(url: String, user: String, schema: String, pass: String): DBConf = {
    new DBConf(url, user, schema, Some(pass))
  }

  def createHikariDataSource(url: String, user: String, pass: String, callback: String => Unit)
  : Option[HikariDataSource] = {

    val config = new HikariConfig()

    config.setDriverClassName("oracle.jdbc.OracleDriver")

    config.setJdbcUrl(url)
    config.setUsername(user)
    config.setPassword(pass)

    config.setMaximumPoolSize(15)          // todo : modify
    config.setMinimumIdle(2)
    config.setConnectionTimeout(30000)     // 30초
    config.setIdleTimeout(600000)

    config.addDataSourceProperty("implicitCachingEnabled", "true")
    config.addDataSourceProperty("oracle.jdbc.defaultNChar", "true")

    config.setInitializationFailTimeout(30000)

    callback( "validate connection in 20 sec.")

    import scala.concurrent.{Future, Await}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    try {
      val f = Future {
        val ds = new HikariDataSource(config)
        val conn = ds.getConnection
        conn.close()
        ds
      }
      Some(Await.result(f, 25.seconds))
    } catch {
      case e: Throwable =>
        callback(bullet + "fail to init connection pool." + e.getMessage)
        None
    }
  }
}

