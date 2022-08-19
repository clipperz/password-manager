import Util._

addCommandAlias("buildAll", "installPurescript; buildPurescript; compile")
addCommandAlias("runAll", "installPurescript; buildPurescript; packagePurescript; run")
addCommandAlias("cleanAll", "clean; cleanDependenciesPurescript")
addCommandAlias(
  "testPurescript",
  "installPurescript; buildPurescript; packageTestPurescript; runTestPurescript",
)
addCommandAlias("testAll", "test; testPurescript")

addCommandAlias("b", "buildPurescript")
addCommandAlias("c", "compile")
addCommandAlias("t", "testAll")
addCommandAlias("r", "runAll")

addCommandAlias(
  "styleCheck",
  "scalafmtSbtCheck; scalafmtCheckAll",
)
addCommandAlias(
  "styleFix",
  "scalafmtSbt; scalafmtAll",
)
addCommandAlias(
  "up2date",
  "reload plugins; dependencyUpdates; reload return; dependencyUpdates",
)

onLoadMessage +=
  s"""|
      |╭────────────────────────────────────╮
      |│     List of defined ${styled("aliases")}        │
      |├────────────────┬───────────────────┤
      |│ ${styled("buildAll")}       │ build all         │
      |│ ${styled("cleanAll")}       │ clean all         │
      |│ ${styled("b")}              │ build purescript  │
      |│ ${styled("c")}              │ compile scala     │
      |│ ${styled("r")}              │ run all           │
      |│ ${styled("t")}              │ test all          │
      |│ ${styled("testPurescript")} │ test purescript   │
      |│ ${styled("styleCheck")}     │ fmt check         │
      |│ ${styled("styleFix")}       │ fmt               │
      |│ ${styled("up2date")}        │ dependencyUpdates │
      |╰────────────────┴───────────────────╯""".stripMargin
