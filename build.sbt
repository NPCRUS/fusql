ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val lib = (project in file("lib"))
  .settings(
    name := "frontend-sql",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % "test" // TODO: use munit
    )
  )

lazy val example = (project in file("example"))
  .settings(
    name := "example",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.2",
      "org.postgresql" % "postgresql" % "42.7.3"
    )
  ).dependsOn(lib)
