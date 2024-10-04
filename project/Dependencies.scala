import sbt._

object Dependencies {
  type Version = String
  type Modules = Seq[ModuleID]

  object Versions {
    val http4s: Version        = "1.0.0-M40"
    val fs2: Version           = "3.10.2"
    val decline: Version       = "2.4.1"
    val log4cats: Version      = "2.7.0"
    val scalaTest: Version     = "3.2.19"
    val doobie: Version        = "1.0.0-RC5"
    val sentryLogback: Version = "7.12.1"
    val ical4j: Version        = "4.0.1"
    val quartz: Version        = "2.3.2"
    val zio: Version           = "2.1.9"
    val zioLogging: Version    = "2.3.1"
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
    "dev.zio" %% "zio-http"    % "3.0.1",
    "dev.zio" %% "zio-cli"     % "0.5.0"
  ) ++ Seq(
    "dev.zio" %% "zio-logging",
    "dev.zio" %% "zio-logging-slf4j2"
  ).map(_ % Versions.zioLogging) ++ Seq(
    "ch.qos.logback" % "logback-classic" % "1.5.7"
  ) ++ Seq(
    "dev.zio" %% "zio-schema",
    "dev.zio" %% "zio-schema-json",
    "dev.zio" %% "zio-schema-zio-test",
    "dev.zio" %% "zio-schema-derivation"
    // "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided" // Needed ?
  ).map(_ % "1.5.0") ++ Seq(
    "dev.zio" %% "zio-metrics-connectors",
    "dev.zio" %% "zio-metrics-connectors-prometheus"
  ).map(_ % "2.3.1")

  lazy val logging: Modules = Seq(
    "ch.qos.logback" % "logback-classic" % "1.5.6"
  ) ++ Seq(
    "org.typelevel" %% "log4cats-core",
    "org.typelevel" %% "log4cats-slf4j"
  ).map(_ % Versions.log4cats) ++ Seq(
    "io.sentry" % "sentry-logback" % Versions.sentryLogback
  )

  lazy val scalaTags: Modules = Seq(
    "com.lihaoyi" %% "scalatags" % "0.13.1"
  )

  lazy val quartz: Modules = Seq(
    "org.quartz-scheduler" % "quartz"
  ).map(_ % Versions.quartz)

  lazy val projectResolvers: Seq[MavenRepository] = Seq(
    // Resolver.sonatypeOssRepos("snapshots"),
    "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases",
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype staging" at "https://oss.sonatype.org/content/repositories/staging",
    "Java.net Maven2 Repository" at "https://download.java.net/maven/2/"
  )

  lazy val crypto: Modules = Seq(
    "io.github.felipebonezi" % "cipherizy-lib" % "1.2.0"
  )
}
