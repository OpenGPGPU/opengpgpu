import $ivy.`com.goyeau::mill-scalafix_mill0.11:0.3.1`
import com.goyeau.mill.scalafix.ScalafixModule
import coursier.maven.MavenRepository

import mill._
import scalalib._
import scalafmt._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.dependencies.cde.common
import $file.`rocket-chip`.dependencies.hardfloat.common
import $file.`rocket-chip`.dependencies.diplomacy.common
import $file.common


val defaultScalaVersion = "2.13.12"


def defaultVersions(chiselVersion: String) = chiselVersion match {
  case "chisel" => Map(
    "chisel"        -> ivy"org.chipsalliance::chisel:6.1.0",
    "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.1.0",
    "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:6.0.0",
    "sourcecode"    -> ivy"com.lihaoyi::sourcecode:0.3.1"
  )
}

trait HasChisel extends ScalaModule with Cross.Module[String] {


  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"))
  }

  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader", "-Ywarn-unused")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object macros extends Macros

trait Macros
  extends millbuild.`rocket-chip`.common.MacrosModule
    with SbtModule {

  def scalaVersion: T[String] = T(defaultScalaVersion)

  def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
}

object hardfloat extends Cross[Hardfloat]("chisel")

trait Hardfloat
  extends millbuild.`rocket-chip`.dependencies.hardfloat.common.HardfloatModule with HasChisel with SbtModule {

  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "rocket-chip" / "dependencies" / "hardfloat" / "hardfloat"

}

object cde extends CDE

trait CDE extends millbuild.`rocket-chip`.dependencies.cde.common.CDEModule with ScalaModule {

  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "rocket-chip" / "dependencies" / "cde" / "cde"
}

object diplomacy extends Cross[Diplomacy]("chisel")

trait Diplomacy
    extends millbuild.`rocket-chip`.dependencies.diplomacy.common.DiplomacyModule
    with HasChisel {

  override def scalaVersion: T[String] = T(defaultScalaVersion)

  def cdeModule = cde

  def sourcecodeIvy = defaultVersions(crossValue)("sourcecode")

  override def millSourcePath = os.pwd / "rocket-chip" / "dependencies" / "diplomacy" / "diplomacy"
}  

object rocketchip extends Cross[RocketChip]("chisel")

trait RocketChip
  extends millbuild.`rocket-chip`.common.RocketChipModule
    with HasChisel with SbtModule {

  override def millSourcePath = os.pwd / "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat(crossValue)

  def cdeModule = cde

  def diplomacyModule = diplomacy(crossValue)

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.4"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.6"
}

object ogpu extends Cross[OGPU]("chisel")

trait OGPU extends millbuild.common.OGPUModule
    with HasChisel
    with SbtModule
    with ScalafixModule
    with ScalafmtModule {

  override def millSourcePath = os.pwd

  def rocketModule = rocketchip(crossValue)

  override def forkArgs = Seq("-Xmx8G", "-Xss256m")

  override def sources = T.sources {
    super.sources() ++ Seq(PathRef(this.millSourcePath / "src" / crossValue / "main" / "scala"))
  }

  def lineCount = T {
    this.sources().filter(ref => os.exists(ref.path)).flatMap(ref => os.walk(ref.path)).filter(os.isFile).flatMap(os.read.lines).size
  }

  def printLineCount() = T.command {
    println(s"Lines of code(LOC): ${lineCount()} !!!")
  }

  object test extends SbtModuleTests
      with TestModule.ScalaTest with ScalafixModule
      with ScalafmtModule {

    override def forkArgs = Seq("-Xmx8G", "-Xss256m") 

    override def sources = T.sources {
      super.sources() ++ Seq(PathRef(this.millSourcePath / "src" / crossValue / "test" / "scala"))
    }

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions(crossValue)("chiseltest")
    )
  }
}
