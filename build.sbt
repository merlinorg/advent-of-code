ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

Global / excludeLintKeys += idePackagePrefix

Test / parallelExecution := false

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name             := "aoc",
    idePackagePrefix := Some("org.merlin.aoc"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
      "org.scalatest"          %% "scalatest"                  % "3.2.19" % "test",
      "com.github.vagmcs"      %% "optimus"                    % "3.4.5",
      "com.github.vagmcs"      %% "optimus-solver-oj"          % "3.4.5",
      "com.github.vagmcs"      %% "optimus-solver-lp"          % "3.4.5"
    ),
    scalacOptions ++= Seq("-deprecation", "-source:future", "-Werror", "-feature", "-new-syntax"),
    javaOptions += "-Xmx16G",
  )
