name := "cs444-joos-compiler"

version := "0.1"

scalaVersion := "2.12.4"

// show all warnings during compilation
scalacOptions ++= Seq("-deprecation", "-feature")

// allow for the killing of the run without killing sbt
//fork in run := true

// assert java 8
initialize := {
  val _ = initialize.value // run the previous initialization
  val required = "1.8"
  val current  = sys.props("java.specification.version")
  assert(current == required, s"Unsupported JDK: java.specification.version $current != $required")
}

// fetch akka library
lazy val akkaVersion = "2.5.3"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
