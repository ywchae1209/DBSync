package schema

object DBUtil {

  case class DBConf( url: String, user: String, schemaName: String, pass: String)

  import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

  def createHikariDataSource( dbconf: DBConf): HikariDataSource = {
    val config = new HikariConfig()

    config.setDriverClassName("oracle.jdbc.OracleDriver")

    // java -jar DBSync_0.1.0.jar -Doracle.jdbc.RetainV9LongBindBehavior=true -Doracle.jdbc.convertNlobToString=false

    // [LONG타입의 ORA-22835 우회 try] --> failed
//    val connectionProperties = "oracle.jdbc.RetainV9LongBindBehavior=true&oracle.jdbc.convertNlobToString=false"
//    val finalUrl = if (dbconf.url.contains("?")) s"${dbconf.url}&$connectionProperties" else s"${dbconf.url}?$connectionProperties"

    config.setJdbcUrl(dbconf.url)
    config.setUsername(dbconf.user)
    config.setPassword(dbconf.pass)

    config.setMaximumPoolSize(15)          // todo : modify
    config.setMinimumIdle(2)
    config.setConnectionTimeout(30000)     // 30초
    config.setIdleTimeout(600000)

    config.addDataSourceProperty("implicitCachingEnabled", "true")
    config.addDataSourceProperty("oracle.jdbc.defaultNChar", "true")

    // todo :: invalid input early check
    config.setInitializationFailTimeout(30000)

    // [LONG타입의 ORA-22835 우회 try] --> failed
//    config.addDataSourceProperty("oracle.jdbc.RetainV9LongBindBehavior", "true")

    new HikariDataSource(config)
  }
}
