name := "Lisp Interpretter."

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// Fork JVM when `run`-ing SBT
// http://stackoverflow.com/a/5265162/409976
fork in run := true