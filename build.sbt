ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val lib = (project in file("lib"))
  .settings(
    name := "frontend-sql",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.0",
      "org.scalatest" %% "scalatest" % "3.2.18" % "test" // TODO: use munit
    )
  )

lazy val example = (project in file("example"))
  .settings(
    name := "example",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.2",
      "com.typesafe.slick" %% "slick" % "3.5.1",
      "org.slf4j" % "slf4j-nop" % "2.0.13",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1",
      "org.postgresql" % "postgresql" % "42.7.3"
    )
  ).dependsOn(lib)
