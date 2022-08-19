ThisBuild / scalaVersion := "2.12.15"
ThisBuild / useSuperShell := false
ThisBuild / autoStartServer := false

update / evictionWarningOptions := EvictionWarningOptions.empty

addDependencyTreePlugin

addSbtPlugin("com.timushev.sbt" % "sbt-rewarn" % "0.1.3")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")
// addSbtPlugin("com.eed3si9n"     % "sbt-assembly" % "0.15.0")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
