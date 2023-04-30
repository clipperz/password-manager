import complete.DefaultParsers._
import Dependencies._
import java.nio.file.Paths

lazy val installPurescript = TaskKey[Unit]("installPurescript", "Install purescript")
installPurescript := {
  import sys.process._
  "npm install" !
}

lazy val buildPurescript = TaskKey[Unit]("buildPurescript", "Build frontend")
buildPurescript := {
  import sys.process._
  "npm run build" !
}

// lazy val keepBuildingPurescript = TaskKey[Unit]("keepBuildingPurescript", "Keep building frontend")
// keepBuildingPurescript := {
//   import sys.process._
//   "npm run keep-building" !
// }

lazy val packagePurescript = TaskKey[Unit]("packagePurescript", "Package frontend")
packagePurescript := {
  import sys.process._
  Process(Seq("bash", "-c", "npm run package"), None, "CURRENT_COMMIT" -> "development").!
}

lazy val keepPackagingPurescript = TaskKey[Unit]("keepPackagingPurescript", "Keep packaging frontend")
keepPackagingPurescript := {
  import sys.process._
  "npm run keep-packaging" !
}

lazy val servePurescript = TaskKey[Unit]("servePurescript", "Serve frontend")
servePurescript := {
  import sys.process._
  "npm run go" !
}

lazy val cleanDependenciesPurescript =
  TaskKey[Unit]("cleanDependenciesPurescript", "Clean dependencies frontend")
cleanDependenciesPurescript := {
  import sys.process._
  "npm run nuke" !
}

lazy val packageTestPurescript = TaskKey[Unit]("packageTestPurescript", "Package test frontend")
packageTestPurescript := {
  import sys.process._
  "npm run test-package" !
}

lazy val runTestPurescript = TaskKey[Unit]("runTestPurescript", "Run test frontend")
runTestPurescript := {
  import sys.process._
  "npm run test-browser" !
}

lazy val cleanTargetSubdir = inputKey[Unit]("Clean the given subdirectory of the target directory")
cleanTargetSubdir := {
  import sys.process._
  val directory: String = spaceDelimited("<arg>").parsed(0)
  val path = Paths.get(baseDirectory.value.toString(), "target", directory)
  s"rm -rf ${path}" !
}

//=====================================================================

ThisBuild / organization := "is.clipperz"
ThisBuild / scalaVersion := "3.2.1"

ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Ykind-projector",
    "-Ysafe-init", // experimental (I've seen it cause issues with circe)
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")

lazy val `clipperz-backend` =
  project
    .in(file("."))
    .settings(name := "clipperz backend")
    .settings(commonSettings)
    .settings(dependencies)
    .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val commonSettings = commonScalacOptions ++ Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty
)

lazy val commonScalacOptions = Seq(
  Compile / console / scalacOptions --= Seq(
    "-Wunused:_",
    "-Xfatal-warnings",
  ),
  Test / console / scalacOptions :=
    (Compile / console / scalacOptions).value,
)

val zio_version = "2.0.13"
// val zio_http_version = "2.0.0-RC11"
val zio_http_version = "3.0.0-RC1"
val zio_logging_version = "2.1.12"
val zio_json = "0.5.0"

lazy val dependencies = Seq(
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zio_version,
    "dev.zio" %% "zio-streams" % zio_version,
    "dev.zio" %% "zio-json" % zio_json,
    "dev.zio" %% "zio-cache" % "0.2.0",
    // "io.d11" %% "zhttp" % zio_http_version,
    "dev.zio" %% "zio-http" % zio_http_version,
    "dev.zio" %% "zio-logging"       % zio_logging_version,
    "dev.zio" %% "zio-logging-slf4j" % zio_logging_version,
    "org.slf4j" % "slf4j-simple" % "1.7.36",
  ),
  libraryDependencies ++= Seq(
    // org.scalatest.scalatest,
    // org.scalatestplus.`scalacheck-1-15`,
    "dev.zio" %% "zio-test" % zio_version,
    "dev.zio" %% "zio-test-sbt" % zio_version,
  ).map(_ % Test),
)

cancelable in Global := true
fork in Global := true

// enablePlugins(JavaAppPackaging)
// enablePlugins(DockerPlugin)
// enablePlugins(AshScriptPlugin)
// dockerBaseImage       := "openjdk:jre-alpine"

Compile / mainClass := Some("is.clipperz.backend.Main")

assemblyJarName in assembly := "clipperz.jar"
assemblyMergeStrategy in assembly := {
 case PathList("META-INF", _*) => MergeStrategy.discard
 case _                        => MergeStrategy.first
}
