name := "training-fp"

version := "0.1"

scalaVersion := "3.7.3"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test"
)

scalacOptions ++= Seq("-Yexplicit-nulls", "-Wsafe-init")
