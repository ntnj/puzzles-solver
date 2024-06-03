Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.5.0-RC2"

ThisBuild / scalacOptions ++= Seq("-experimental", "-deprecation", "-explain", "-feature")

ThisBuild / libraryDependencies += "tools.aqua" % "z3-turnkey" % "4.13.0"

ThisBuild / publish / skip := true

lazy val z3s = project
  .settings(
    publish / skip := false,
    name := "z3s",
    version := "0.0.1",
    organization := "io.github.ntnj",
    description := "Scala wrapper over Z3",
    homepage := Some(url("https://github.com/ntnj/puzzles-solver/tree/master/scala/z3s")),
    developers := List(Developer("ntnj", "Nitin Jain", "", url("https://nitinja.in"))),
    scmInfo := Some(ScmInfo(url("https://github.com/ntnj/puzzles-solver"), "scm:git:https://github.com/ntnj/puzzles-solver.git")),
    licenses := Seq("MIT" -> url("https://github.com/ntnj/puzzles-solver/tree/master/LICENSE")),
    versionScheme := Some("semver-spec")
  )

lazy val puzzles = project
  .dependsOn(z3s)
