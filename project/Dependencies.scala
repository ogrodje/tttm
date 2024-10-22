import sbt._

object Dependencies {
  type Version = String
  type Modules = Seq[ModuleID]

  object Versions {
    val log4cats: Version      = "2.7.0"
    val scalaTest: Version     = "3.2.19"
    val doobie: Version        = "1.0.0-RC6"
    val sentryLogback: Version = "7.12.1"
    val ical4j: Version        = "4.0.1"
    val quartz: Version        = "2.3.2"
    val zio: Version           = "2.1.11"
    val zioLogging: Version    = "2.3.2"
    val zioHttp: Version       = "3.0.1"
    val postgresql: Version    = "42.7.4"
    val flyway: Version        = "10.20.0"
  }

  lazy val zio: Modules = Seq(
    "dev.zio" %% "zio",
    "dev.zio" %% "zio-streams"
  ).map(_ % Versions.zio) ++ Seq(
    "dev.zio" %% "zio-test",
    "dev.zio" %% "zio-test-sbt",
    "dev.zio" %% "zio-test-magnolia"
  ).map(_ % Versions.zio % Test) ++ Seq(
    "dev.zio" %% "zio-prelude" % "1.0.0-RC31",
    "dev.zio" %% "zio-cli"     % "0.5.0"
  ) ++ Seq(
    "dev.zio" %% "zio-logging",
    "dev.zio" %% "zio-logging-slf4j2"
  ).map(_ % Versions.zioLogging) ++ Seq(
    "ch.qos.logback" % "logback-classic" % "1.5.11"
  ) ++ Seq(
    "dev.zio" %% "zio-schema",
    "dev.zio" %% "zio-schema-json",
    "dev.zio" %% "zio-schema-zio-test",
    "dev.zio" %% "zio-schema-derivation"
    // "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided" // Needed ?
  ).map(_ % "1.5.0") ++ Seq(
    "dev.zio" %% "zio-metrics-connectors",
    "dev.zio" %% "zio-metrics-connectors-prometheus"
  ).map(_ % "2.3.1") ++ Seq(
    "dev.zio" %% "zio-json-yaml" % "0.7.3"
  ) ++ Seq(
    "eu.timepit" %% "refined" % "0.11.2"
  ) ++ Seq(
    "dev.zio" %% "zio-http"         % Versions.zioHttp,
    "dev.zio" %% "zio-http-testkit" % Versions.zioHttp % Test
  )

  lazy val logging: Modules = Seq(
    "ch.qos.logback" % "logback-classic" % "1.5.9"
  ) ++ Seq(
    "org.typelevel" %% "log4cats-core",
    "org.typelevel" %% "log4cats-slf4j"
  ).map(_ % Versions.log4cats) ++ Seq(
    "io.sentry" % "sentry-logback" % Versions.sentryLogback
  )

  lazy val quartz: Modules = Seq(
    "org.quartz-scheduler" % "quartz"
  ).map(_ % Versions.quartz)

  lazy val db: Modules = Seq(
    "org.tpolecat" %% "doobie-core",
    "org.tpolecat" %% "doobie-hikari",  // HikariCP transactor.
    "org.tpolecat" %% "doobie-postgres" // Postgres driver
    // "org.tpolecat" %% "doobie-postgres-circe"
  ).map(_ % Versions.doobie) ++ Seq(
    "org.postgresql" % "postgresql" % Versions.postgresql
  ) ++ Seq(
    "org.flywaydb" % "flyway-core",
    "org.flywaydb" % "flyway-database-postgresql"
  ).map(_ % Versions.flyway) ++ Seq(
    "dev.zio" %% "zio-interop-cats" % "23.1.0.3"
  )

  lazy val projectResolvers: Seq[MavenRepository] = Seq(
    // Resolver.sonatypeOssRepos("snapshots"),
    "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases",
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype staging" at "https://oss.sonatype.org/content/repositories/staging",
    "Java.net Maven2 Repository" at "https://download.java.net/maven/2/"
  )
}
