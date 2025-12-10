ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

Global / excludeLintKeys += idePackagePrefix

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name             := "aoc",
    idePackagePrefix := Some("org.merlin.aoc"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
      "org.scalatest"          %% "scalatest"                  % "3.2.19" % "test",
      "org.apache.commons"      % "math3"                      % "3.6.1"
    ),
    scalacOptions ++= Seq("-deprecation", "-source:future", "-Werror", "-feature", "-new-syntax"),
    javaOptions += "-Xmx16G",
  )
