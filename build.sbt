organization  := "com.synnks"
homepage      := Some(uri("https://github.com/synnks/atoms"))
licenses      := List(License.Apache2)
developers    := List(
  Developer(
    "synnks",
    "Daniel Stanila",
    "daniel.stanila@synnks.com",
    uri("https://github.com/synnks")
  )
)
versionScheme := Some("early-semver")

val Scala213Version = "2.13.18"
val Scala3Version   = "3.9.0"

scalaVersion       := Scala213Version
crossScalaVersions := Seq(Scala213Version, Scala3Version)

val CatsCoreVersion        = "2.13.0"
val ShapelessVersion       = "2.3.13"
val MUnitVersion           = "1.3.6"
val MUnitScalaCheckVersion = "1.3.1"

val Jackson2Version = "2.22.4"
val Jackson3Version = "3.2.4"
val JLineVersion    = "4.4.8"
val JsoupVersion    = "1.23.2"

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

val scalaVersionSpecificScalacOptions = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => scala2ScalacOptions
    case ScalaVersionKind.Scala3 => scala3ScalacOptions
  }
}

val scalaVersionSpecificToolingDependencies = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => Nil
    case ScalaVersionKind.Scala3 =>
      Seq(
        ("com.fasterxml.jackson" % "jackson-bom"  % Jackson2Version % "scala-doc-tool").pomOnly(),
        ("tools.jackson"         % "jackson-bom"  % Jackson3Version % "scala-doc-tool").pomOnly(),
        ("org.jline"             % "jline-parent" % JLineVersion    % "scala-repl-tool").pomOnly()
      )
  }
}

val scalaVersionSpecificToolingOverrides = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => Nil
    case ScalaVersionKind.Scala3 => Seq("org.jsoup" % "jsoup" % JsoupVersion)
  }
}

val hlistBackendDependencies = Def.setting {
  ScalaVersionKind.from(scalaVersion.value) match {
    case ScalaVersionKind.Scala2 => Seq("com.chuusai" %% "shapeless" % ShapelessVersion)
    case ScalaVersionKind.Scala3 => Nil
  }
}

val commonScalacSettings = Seq(
  scalacOptions ++= commonScalacOptions ++ scalaVersionSpecificScalacOptions.value
)

// Keep Scaladoc and REPL dependency families aligned on patched versions.
val commonToolingSettings = Seq(
  libraryDependencies ++= scalaVersionSpecificToolingDependencies.value,
  dependencyOverrides ++= scalaVersionSpecificToolingOverrides.value
)

val commonTestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"            % MUnitVersion           % Test,
    "org.scalameta" %% "munit-scalacheck" % MUnitScalaCheckVersion % Test
  )
)

val hlistSettings = commonScalacSettings ++ commonToolingSettings ++ Seq(
  libraryDependencies ++= hlistBackendDependencies.value,
  publish / skip := true
)

lazy val root = (project in file("."))
  .aggregate(core, hlist)
  .settings(commonToolingSettings)
  .settings(
    name                          := "atoms-root",
    publish / skip                := true,
    scalafmtRepo / aggregate      := false,
    scalafmtCheckRepo / aggregate := false
  )

lazy val hlist = (project in file("hlist"))
  .settings(hlistSettings)
  .settings(
    name := "atoms-hlist"
  )

val coreSettings = commonScalacSettings ++ commonToolingSettings ++ commonTestSettings ++ Seq(
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
