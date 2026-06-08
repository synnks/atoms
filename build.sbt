ThisBuild / organization  := "com.synnks"
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

val Scala213Version = "2.13.18"

ThisBuild / scalaVersion       := Scala213Version
ThisBuild / crossScalaVersions := Seq(Scala213Version)

val CatsCoreVersion        = "2.13.0"
val ShapelessVersion       = "2.3.13"
val MUnitVersion           = "1.3.2"
val MUnitScalaCheckVersion = "1.3.0"

val commonScalacOptions = Seq(
  "-encoding",
  "utf8",
  "-feature",
  "-language:implicitConversions,higherKinds",
  "-deprecation",
  "-unchecked",
  "-Wnonunit-statement",
  "-Wvalue-discard"
)

val scala2ScalacOptions = Seq(
  "-Xlint:implicit-recursion",
  "-Xfatal-warnings",
  "-Xsource:3"
)

val scalaVersionSpecificScalacOptions = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => scala2ScalacOptions
    case ScalaVersionKind.Scala3 => Nil
  }
}

val scalaVersionSpecificDependencies = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => Seq("com.chuusai" %% "shapeless" % ShapelessVersion)
    case ScalaVersionKind.Scala3 => Nil
  }
}

val commonScalacSettings = Seq(
  scalacOptions ++= commonScalacOptions ++ scalaVersionSpecificScalacOptions.value
)

val commonTestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"            % MUnitVersion           % Test,
    "org.scalameta" %% "munit-scalacheck" % MUnitScalaCheckVersion % Test
  )
)

val coreSettings = commonScalacSettings ++ commonTestSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % CatsCoreVersion
  ) ++ scalaVersionSpecificDependencies.value,
  coverageEnabled := false
)

val hlistSettings = commonScalacSettings ++ Seq(
  libraryDependencies ++= scalaVersionSpecificDependencies.value,
  publish / skip := true
)

lazy val root = (project in file("."))
  .aggregate(core, hlist)
  .settings(
    name           := "atoms",
    publish / skip := true
  )

lazy val hlist = (project in file("hlist"))
  .settings(hlistSettings)
  .settings(
    name := "atoms-hlist"
  )

lazy val core = (project in file("core"))
  .settings(coreSettings)
  .settings(
    name        := "atoms",
    description := "Type-safe data cube for Scala"
  )
