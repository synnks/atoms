ThisBuild / organization := "com.synnks"

ThisBuild / scalaVersion := "2.13.18"

ThisBuild / homepage      := Some(url("https://github.com/synnks/atoms"))
ThisBuild / licenses      := List(License.Apache2)
ThisBuild / developers    := List(
  Developer(
    "synnks",
    "Daniel Stanila",
    "daniel.stanila@synnks.com",
    url("https://github.com/synnks")
  )
)
ThisBuild / versionScheme := Some("early-semver")

val CatsCoreVersion        = "2.13.0"
val ShapelessVersion       = "2.3.13"
val MUnitVersion           = "1.3.2"
val MUnitScalaCheckVersion = "1.3.0"

lazy val root = (project in file("."))
  .aggregate(core)
  .settings(
    publish / skip := true
  )

lazy val core = (project in file("core"))
  .settings(
    name        := "atoms",
    description := "Type-safe data cube for Scala"
  )

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"        % CatsCoreVersion,
  "com.chuusai"   %% "shapeless"        % ShapelessVersion,
  "org.scalameta" %% "munit"            % MUnitVersion           % Test,
  "org.scalameta" %% "munit-scalacheck" % MUnitScalaCheckVersion % Test
)

ThisBuild / coverageEnabled := false

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-feature",
  "-language:implicitConversions,higherKinds",
  "-deprecation",
  "-unchecked",
  "-Wnonunit-statement",
  "-Wvalue-discard",
  "-Xlint:implicit-recursion",
  "-Xfatal-warnings",
  "-Xsource:3"
)
