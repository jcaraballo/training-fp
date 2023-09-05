name := "training-fp"

version := "0.1"

scalaVersion := "3.3.0"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.16" % "test",
  "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
)

scalacOptions ++= Seq("-Yexplicit-nulls", "-Ysafe-init")
