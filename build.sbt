import Dependencies._

lazy val installPurescript = TaskKey[Unit]("installPurescript", "Install purescript")
installPurescript := {
  import sys.process._
  "yarn install" !
}

lazy val buildPurescript = TaskKey[Unit]("buildPurescript", "Build frontend")
buildPurescript := {
  import sys.process._
  "yarn build" !
}

lazy val packagePurescript = TaskKey[Unit]("packagePurescript", "Package frontend")
packagePurescript := {
  import sys.process._
  "yarn package" !
}

lazy val servePurescript = TaskKey[Unit]("servePurescript", "Serve frontend")
servePurescript := {
  import sys.process._
  "yarn go" !
}

lazy val cleanDependenciesPurescript =
  TaskKey[Unit]("cleanDependenciesPurescript", "Clean dependencies frontend")
cleanDependenciesPurescript := {
  import sys.process._
  "yarn nuke" !
}

lazy val packageTestPurescript = TaskKey[Unit]("packageTestPurescript", "Package test frontend")
packageTestPurescript := {
  import sys.process._
  "yarn test-package" !
}

lazy val runTestPurescript = TaskKey[Unit]("runTestPurescript", "Run test frontend")
runTestPurescript := {
  import sys.process._
  "yarn test-browser" !
}

//=====================================================================

ThisBuild / organization := "is.clipperz"
ThisBuild / scalaVersion := "3.1.2"

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

val zio_version = "2.0.0"
val zio_http_version = "2.0.0-RC10"
val zio_json = "0.3.0-RC11"

lazy val dependencies = Seq(
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zio_version,
    "dev.zio" %% "zio-streams" % zio_version,
    "dev.zio" %% "zio-json" % zio_json,
    "io.d11" %% "zhttp" % zio_http_version,
  ),
  libraryDependencies ++= Seq(
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-15`,
    "dev.zio" %% "zio-test" % zio_version,
    "dev.zio" %% "zio-test-sbt" % zio_version,
  ).map(_ % Test),
)

cancelable in Global := true
fork in Global := true
