import sbt.Keys._

object Common {

  val settings =
    Seq(
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.12.4",
      crossScalaVersions := Seq("2.11.11")
    )

}
