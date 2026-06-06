ThisBuild / organization := "com.synnks"

ThisBuild / scalaVersion       := "2.13.18"
ThisBuild / crossScalaVersions := Seq("2.13.18", "3.8.3")

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
    name           := "atoms",
    publish / skip := true
  )

lazy val core = (project in file("core"))
  .settings(
    name        := "atoms",
    description := "Type-safe data cube for Scala"
  )

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"        % CatsCoreVersion,
  "org.scalameta" %% "munit"            % MUnitVersion           % Test,
  "org.scalameta" %% "munit-scalacheck" % MUnitScalaCheckVersion % Test
) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, _)) => Seq("com.chuusai" %% "shapeless" % ShapelessVersion)
  case _            => Nil
})

ThisBuild / coverageEnabled := false

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-feature",
  "-language:implicitConversions,higherKinds",
  "-deprecation",
  "-unchecked"
) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, _)) =>
    Seq(
      "-Wnonunit-statement",
      "-Wvalue-discard",
      "-Xlint:implicit-recursion",
      "-Xfatal-warnings",
      "-Xsource:3"
    )
  case _            => Nil
})
