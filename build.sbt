import scala.collection.Seq

ThisBuild / version := "0.9.9"

ThisBuild / scalaVersion := "2.13.18"

lazy val root = (project in file("."))
  .settings(
    name := "DBSync"
  )

fork / run := true
assembly / assemblyJarName := s"${name.value}_${version.value}.jar"


// assembly merge strategy....
////////////////////////////////////////////////////////////////////////////////
//assembly / assemblyMergeStrategy := {
//  case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
//  case PathList("META-INF", xs @ _*)             => MergeStrategy.discard
//  case x => MergeStrategy.first
//}

assembly / assemblyMergeStrategy := {
  // 1. 서비스 로더 파일들은 하나로 합침 (기존 설정 유지)
  case PathList("META-INF", "services", _ @ _*) => MergeStrategy.concat

  // 2. JLine 및 네이티브 라이브러리 관련 설정 파일 보존 (중요!)
  // discard 대신 filterContents나 first를 사용하여 파일이 사라지지 않게 합니다.
  case PathList("META-INF", "org", "jline", _ @ _*) => MergeStrategy.first

  // 3. 라이선스나 불필요한 파일만 버림
  case PathList("META-INF", xs @ _*) =>
    xs map { _.toLowerCase } match {
      case "manifest.mf" :: Nil | "index.list" :: Nil | "dependencies" :: Nil => MergeStrategy.discard
      case ps if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") || ps.last.endsWith(".rsa") => MergeStrategy.discard
      case _ => MergeStrategy.first // 그 외 META-INF 파일들은 일단 유지
    }

  case x => MergeStrategy.first
}

libraryDependencies ++= Seq(
  "org.jline" % "jline" % "4.0.12",
    "org.jline" % "jline-terminal" % "4.0.12",
    "org.jline" % "jline-terminal-jni" % "4.0.12",
    "org.jline" % "jline-terminal-ffm" % "4.0.12",
)

// DB connection pool
libraryDependencies += "com.zaxxer" % "HikariCP" % "7.0.0"

// oracle
val oraVer = "23.26.2.0.0" // "23.8.0.25.04"
libraryDependencies += "com.oracle.database.jdbc" % "ojdbc11" % oraVer
libraryDependencies += "com.oracle.database.nls" % "orai18n" % oraVer
libraryDependencies += "com.oracle.database.xml" % "xdb" % oraVer
libraryDependencies += "com.oracle.database.xml" % "xmlparserv2" % oraVer

// oracle Spatial Geometry (including JGeometry)
libraryDependencies += "com.oracle.database.spatial" % "sdoutl" % "23.26"
libraryDependencies += "com.oracle.database.spatial" % "sdoapi" % "23.26"

// mysql
//libraryDependencies += "com.mysql" % "mysql-connector-j" % "9.3.0"

// postgreSql
//libraryDependencies += "org.postgresql" % "postgresql" % "42.7.7"

// no-logging
//libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.12"
// logging
libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "2.0.18",
  "org.apache.logging.log4j" % "log4j-api" % "2.23.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.23.1",
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.23.1", // SLF4J → Log4j2 연결
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

// zio json
libraryDependencies += "dev.zio" %% "zio-json" % "0.9.2"
libraryDependencies += "dev.zio" %% "zio" % "2.1.26"

// messagePack
libraryDependencies += "org.msgpack" % "msgpack-core" % "0.9.12"