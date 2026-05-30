package schema

object DBUtil {

  case class DBConf( url: String, user: String, pass: String)

  import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

  def createHikariDataSource( dbconf: DBConf): HikariDataSource = {
    val config = new HikariConfig()

    config.setDriverClassName("oracle.jdbc.OracleDriver")
    config.setJdbcUrl(dbconf.url)
    config.setUsername(dbconf.user)
    config.setPassword(dbconf.pass)

    config.setMaximumPoolSize(5)           // 한 테이블당 1~2개 커넥션만 쓰므로 작게 잡아도 됨
    config.setMinimumIdle(2)
    config.setConnectionTimeout(30000)     // 30초
    config.setIdleTimeout(600000)

    config.addDataSourceProperty("implicitCachingEnabled", "true")
    config.addDataSourceProperty("oracle.jdbc.defaultNChar", "true")

    new HikariDataSource(config)
  }
}
