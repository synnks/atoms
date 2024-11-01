ThisBuild / organization := "com.synnks"

ThisBuild / scalaVersion := "2.13.15"

val CatsCoreVersion  = "2.12.0"
val ShapelessVersion = "2.3.12"
val MUnitVersion     = "1.0.0"

lazy val core = (project in file("core"))
  .settings(moduleName := "atoms")

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"        % CatsCoreVersion,
  "com.chuusai"   %% "shapeless"        % ShapelessVersion,
  "org.scalameta" %% "munit"            % MUnitVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % MUnitVersion % Test
)

ThisBuild / coverageEnabled := true

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
