name := "prfunct"

version := "0.1"

scalaVersion := "2.13.6"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
)

scalacOptions ++= Seq (
  "-Xsource:3"
)
