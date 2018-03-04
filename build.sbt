name := "cs444-joos-compiler"

version := "0.1"

scalaVersion := "2.12.4"

// show all warnings during compilation
scalacOptions ++= Seq("-deprecation", "-feature")

// allow for the killing of the run without killing sbt
fork in run := true

// set program entry point to Main
mainClass in (Compile, run) := Some("Main")
