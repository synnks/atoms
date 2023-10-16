ThisBuild / organization := "com.synnks"
ThisBuild / version      := "0.0.1"

ThisBuild / scalaVersion := "2.13.12"

val CatsCoreVersion  = "2.10.0"
val CatsLawsVersion  = "2.10.0"
val ShapelessVersion = "2.3.10"

lazy val core = (project in file("core"))
  .settings(moduleName := "atoms")

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"        % CatsCoreVersion,
  "com.chuusai"   %% "shapeless"        % ShapelessVersion,
  "org.typelevel" %% "cats-laws"        % CatsLawsVersion % Test,
  "org.typelevel" %% "discipline-munit" % "2.0.0-M3"      % Test
)

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-feature",
  "-language:implicitConversions,higherKinds",
  "-deprecation",
  "-unchecked",
  "-Wnonunit-statement",
  "-Wvalue-discard",
  "-Xlint:unused",
  "-Xlint:implicit-recursion",
  "-Xfatal-warnings",
  "-Xsource:3",
  "-Xlog-implicits"
)
