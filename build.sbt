ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

Global / excludeLintKeys += idePackagePrefix

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name             := "aoc",
    idePackagePrefix := Some("org.merlin.aoc"),
    libraryDependencies ++= Seq(
      "org.scalaz"             %% "scalaz-core"                % "7.3.8",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.scalatest"          %% "scalatest"                  % "3.2.19" % "test",
    ),
    scalacOptions ++= Seq("-deprecation", "-source:future", "-Werror"),
    javaOptions += "-Xmx16G",
  )
