import sbt._
import sbt.Keys._

object Build extends Build {
  val commonDeps = libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test")

  val testSettings = inConfig(ItTest)(Defaults.testTasks) ++ Seq(
    parallelExecution in Test := false,
    parallelExecution in ItTest := false,

    testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a", "-s"), Tests.Filter(unitFilter)),
    testOptions in ItTest := Seq(Tests.Argument(TestFrameworks.JUnit, "-v", "-a", "-s", "-q"), Tests.Filter(itFilter))
  )

  val buildSettings = Seq(
    organization := "com.huawei.scalan",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-unchecked", "-deprecation",
      "-feature",
      "-language:_")
  )

  override lazy val settings = super.settings ++ buildSettings

  lazy val commonSettings =
    buildSettings ++ testSettings ++ commonDeps

  implicit class ProjectExt(p: Project) {
    def allConfigDependency = p % "compile->compile;test->test"

    def addTestConfigsAndCommonSettings =
      p.configs(ItTest).settings(commonSettings: _*)
  }

  def liteDependency(name: String) = "com.huawei.scalan" %% name % "0.2.11-SNAPSHOT"

  lazy val metaDeps = liteDependency("scalan-meta")
  lazy val meta = Project(
    id = "starter-meta",
    base = file("meta")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(metaDeps))

  lazy val library = liteDependency("scalan-library")
  lazy val common = liteDependency("scalan-common")
  lazy val core = liteDependency("scalan-core")
  lazy val ml_study = Project(
    id = "scalan-starter",
    base = file(".")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(library, library % "test" classifier "tests",
      common, common % "test" classifier "tests",
      core, core % "test" classifier "tests"))

  def itFilter(name: String): Boolean =
    name endsWith "ItTests"

  def unitFilter(name: String): Boolean = 
    !itFilter(name)

  lazy val ItTest = config("it").extend(Test)
}
