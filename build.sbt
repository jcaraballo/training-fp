import sbt._

name := "prfunct"

version := "0.1"

scalaVersion := "2.13.3"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
)
