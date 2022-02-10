name := "training-fp"

version := "0.1"

scalaVersion := "3.1.1"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
)

scalacOptions ++= Seq("-Yexplicit-nulls", "-Ysafe-init")
