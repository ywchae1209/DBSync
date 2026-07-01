package schema

import com.zaxxer.hikari.HikariDataSource
import schema.DBConf.HikariDataSourceWithConf
import tui.SyncTUI.bullet
import tui.layoutzEx._
import zio.json.{DeriveJsonCodec, JsonCodec}

case class DBConf(url: String, user: String, schema: String, pass: Option[String] = None) {

  def neqUrlSchema(o: Option[DBConf]) =
  {
    o match {
      case None => true
      case Some(other) =>
        val out = (url != other.url) || (schema != other.schema)
        out
    }
  }

  private var ds: Option[HikariDataSourceWithConf] = None

  def withoutPass = copy(pass = None)

  def initDataSource(kind:String, callback: String => Unit): Boolean
  = {
    close()
    val out = pass
      .orElse{callback(bullet + s"pass is not set($kind)."); None}
      .flatMap( p => DBConf.createHikariDataSource(this, callback) )

    ds = out
    ds.isDefined
  }

  def connected = ds.nonEmpty

  def dataSourceOr(kind: String, callback: String => Unit) = {
    if(ds.isEmpty) callback( bullet + s"connection pool not initialized($kind).")
    ds
  }

  def displayWith(prefix: String, callback: String => Unit): Unit = {
    callback( layout(  prefix,
      table(
        Seq( "url", "schema", "id", "pwd"),
        Seq( Seq( url, schema, user , pass.mkString.map(_ => '*') ) ) ) ).render )
  }

  def display(kind: String, callback: String => Unit): Unit = {
    displayWith(s"-- connection setting for ${kind.yellow}", callback)
  }

  def alreadyInitalized(kind: String, callback: String => Unit) = {
    if(ds.nonEmpty) callback( bullet + s"connection pool already exists($kind).")
    ds.nonEmpty
  }

  def passIsNotSet(kind: String, callback: String => Unit) = {
    pass match {
      case Some(_) => true
      case None    => callback(bullet + s"password is not set($kind). see " + "so ta".color(Color.Green))
        false
    }
  }

  def close() = ds.foreach(_.close())

}

object DBConf {

  def displayToWith(prefix: String, c1: Option[DBConf], c2: Option[DBConf], callback: String => Unit): Unit = {
    if(c1.isEmpty && c2.isEmpty) {
      callback(bullet + "empty")
      return
    }

    val rows= Seq(
      c1.map(c => Seq[Element]("source".green, c.url, c.schema, c.user)),
      c2.map(c => Seq[Element]("target".green, c.url, c.schema, c.user))
    ).flatten

    callback( layout( bullet + prefix,
      table(
        Seq( "", "url", "schema", "id"),
        rows,
      )
    ).render )
  }

  def displayTo(prefix: String, c1: DBConf, c2: DBConf, callback: String => Unit): Unit = {
    displayToWith(prefix, Some(c1), Some(c2), callback)
  }

  implicit val jsonCodec: JsonCodec[DBConf] = DeriveJsonCodec.gen[DBConf]

  import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

  def apply(url: String, user: String, schema: String, pass: String): DBConf = {
    new DBConf(url, user, schema, Some(pass))
  }

  case class HikariDataSourceWithConf(conf:DBConf, hconfig: HikariConfig) extends HikariDataSource(hconfig)

  def createHikariDataSource(conf: DBConf, callback: String => Unit)
  : Option[HikariDataSourceWithConf] = {

    val config = new HikariConfig()

    config.setDriverClassName("oracle.jdbc.OracleDriver")

    config.setJdbcUrl(conf.url)
    config.setUsername(conf.user)
    config.setPassword(conf.pass.get)

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
        val ds = HikariDataSourceWithConf(conf, config)
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

