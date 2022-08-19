import Util._

Global / onChangedBuildSource := ReloadOnSourceChanges

Global / excludeLintKeys ++= Set(
  autoStartServer,
  turbo,
  evictionWarningOptions,
)

Test / parallelExecution := false
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oSD")
Test / turbo := true

ThisBuild / autoStartServer := false
ThisBuild / includePluginResolvers := true
ThisBuild / turbo := true

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

ThisBuild / shellPrompt := { state => s"${prompt(projectName(state))}> " }
ThisBuild / watchStartMessage := {
  case (iteration, ProjectRef(build, projectName), commands) =>
    Some {
      s"""|~${commands.map(styled).mkString(";")}
          |Monitoring source files for ${prompt(projectName)}...""".stripMargin
    }
}
