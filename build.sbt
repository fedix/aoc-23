val scala3Version = "3.4.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc-23",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-language:implicitConversions",
      "-feature",
      "-deprecation",
      "-unchecked",
      // "-rewrite",
      "-source",
      "future-migration",
      // "-Vprofile",
      "-Xfatal-warnings",
      "-Wunused:all",
      "-Wvalue-discard",
      // "-Yexplicit-nulls",
      "-Ysafe-init",
      "-Ykind-projector"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
