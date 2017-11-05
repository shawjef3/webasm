name := "ast"

organization := "me.jeffshaw.webasm"

libraryDependencies ++= Seq(
//  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scodec" %% "scodec-scalaz" % "1.4.1a",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

Common.settings
