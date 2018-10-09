name := "battleship"

version := "0.1"

scalaVersion := "2.12.7"

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
val scalaReflect = "org.scala-lang" % "scala-reflect" % "2.12.7"

libraryDependencies += scalaTest % Test
libraryDependencies += scalaReflect
