import sbt._

name := "prfunct"

version := "0.1"

scalaVersion := "0.26.0-RC1"

//scalacOptions ++= { if (isDotty.value) Seq("-source:3.0-migration") else Nil }
scalacOptions ++= Seq("-source:3.0-migration", "-rewrite")


libraryDependencies ++= Seq(
  ("org.scalatest" %% "scalatest" % "3.2.0" % "test").withDottyCompat(scalaVersion.value),
  ("org.scalacheck" %% "scalacheck" % "1.14.1" % "test").withDottyCompat(scalaVersion.value)
)
