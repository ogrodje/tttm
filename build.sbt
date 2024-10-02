import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.docker.Cmd
import com.typesafe.sbt.packager.docker.DockerPlugin.autoImport._
import Dependencies.*

ThisBuild / version            := "0.0.1"
ThisBuild / scalaVersion       := "3.5.1"
ThisBuild / evictionErrorLevel := Level.Info

lazy val root = (project in file("."))
  .enablePlugins(JavaServerAppPackaging, DockerPlugin)
  .settings(name := "tttm")
  .settings(
    Compile / mainClass := Some("si.ogrodje.tttm.GameServerApp"),
    libraryDependencies ++= zio,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-Yretain-trees",
      "-Xmax-inlines:100"
    )
  )
  .settings(
    assembly / assemblyJarName       := "tttm.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class")                        =>
        MergeStrategy.discard
      case PathList("META-INF", "jpms.args")                    =>
        MergeStrategy.discard
      case PathList("META-INF", "io.netty.versions.properties") =>
        MergeStrategy.first
      case PathList("deriving.conf")                            =>
        MergeStrategy.last
      case PathList(ps @ _*) if ps.last endsWith ".class"       => MergeStrategy.last
      case x                                                    =>
        val old = (assembly / assemblyMergeStrategy).value
        old(x)
    }
  )
  .settings(
    dockerExposedPorts    := Seq(7006),
    dockerExposedUdpPorts := Seq.empty[Int],
    dockerUsername        := Some("ogrodje"),
    dockerUpdateLatest    := true,
    dockerRepository      := Some("ghcr.io"),
    dockerBaseImage       := "azul/zulu-openjdk-alpine:21-latest",
    packageName           := "tttm",
    dockerCommands        := dockerCommands.value.flatMap {
      case add @ Cmd("RUN", args @ _*) if args.contains("id") =>
        List(
          Cmd("LABEL", "maintainer Oto Brglez <otobrglez@gmail.com>"),
          Cmd("LABEL", "org.opencontainers.image.url https://github.com/ogrodje/tttm"),
          Cmd("LABEL", "org.opencontainers.image.source https://github.com/ogrodje/tttm"),
          Cmd("RUN", "apk add --no-cache bash jq curl"),
          Cmd("ENV", "SBT_VERSION", sbtVersion.value),
          Cmd("ENV", "SCALA_VERSION", scalaVersion.value),
          Cmd("ENV", "TTTM_VERSION", version.value),
          add
        )
      case other                                              => List(other)
    }
  )

resolvers ++= Resolver. sonatypeOssRepos("snapshots")