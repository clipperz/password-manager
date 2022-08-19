import sbt._

object Dependencies {
  case object org {
    case object scalatest {
      val scalatest =
        "org.scalatest" %% "scalatest" % "3.2.13"
    }

    case object scalatestplus {
      val `scalacheck-1-15` =
        "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0"
    }
  }
}
