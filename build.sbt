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
val Scala3Version   = "3.8.4"

ThisBuild / scalaVersion       := Scala213Version
ThisBuild / crossScalaVersions := Seq(Scala213Version, Scala3Version)

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

val scala3ScalacOptions = Seq(
  "-Werror"
)

val commonDocScalacOptions = Seq(
  "-encoding",
  "utf8"
)

val scalaVersionSpecificScalacOptions = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => scala2ScalacOptions
    case ScalaVersionKind.Scala3 => scala3ScalacOptions
  }
}

val hlistBackendDependencies = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => Seq("com.chuusai" %% "shapeless" % ShapelessVersion)
    case ScalaVersionKind.Scala3 => Nil
  }
}

val commonScalacSettings = Seq(
  scalacOptions ++= commonScalacOptions ++ scalaVersionSpecificScalacOptions.value,
  Compile / doc / scalacOptions := commonDocScalacOptions
)

val commonTestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"            % MUnitVersion           % Test,
    "org.scalameta" %% "munit-scalacheck" % MUnitScalaCheckVersion % Test
  )
)

val hlistSettings = commonScalacSettings ++ Seq(
  libraryDependencies ++= hlistBackendDependencies.value,
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

val coreSettings = commonScalacSettings ++ commonTestSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % CatsCoreVersion
  ) ++ hlistBackendDependencies.value,
  coverageEnabled := false,
  Compile / packageBin / mappings ++= (hlist / Compile / packageBin / mappings).value,
  Compile / packageSrc / mappings ++= (hlist / Compile / packageSrc / mappings).value
)

lazy val core = (project in file("core"))
  .dependsOn(hlist % "compile-internal, test-internal")
  .settings(coreSettings)
  .settings(
    name        := "atoms",
    description := "Type-safe data cube for Scala"
  )
