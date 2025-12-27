name := "advent-of-code"

version := "0.1"

scalaVersion := "3.7.4"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.20.0",
  "com.softwaremill.sttp.client3" %% "core" % "3.11.0",
  "com.softwaremill.sttp.client3" %% "circe" % "3.11.0",
  "io.circe" %% "circe-generic" % "0.14.15",
  "org.typelevel" %% "cats-core" % "2.13.0"
)
