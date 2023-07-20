import mill._, scalalib._

import mill.scalalib.TestModule.ScalaTest

import $file.`rocket-chip`.build
import $file.`rocket-chip`.common

object rocketchip extends `rocket-chip`.common.CommonRocketChip {
  m =>
  override def scalaVersion: T[String] = T {
    "2.13.10"
  }
  override def ammoniteVersion: T[String] = T {
    "2.4.0"
  }

  val rcPath = os.pwd / "rocket-chip"

  override def millSourcePath = rcPath

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    // use same chisel version with RocketChip
    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      common.getVersion("chisel3")
    ) else Agg.empty[Dep]
  }


  object cdeRocket extends `rocket-chip`.cde.common.CDEModule with PublishModule {
    override def millSourcePath = rcPath / "cde" / "cde"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def pomSettings = T {
      rocketchip.pomSettings()
    }

    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }


  def hardfloatModule = hardfloatRocket

  def cdeModule = cdeRocket
}



object opengpgpu extends SbtModule { m =>
  override def millSourcePath = os.pwd
  override def scalaVersion = "2.13.10"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  )
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.6.0",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0",
  )

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )

  object test extends Tests with ScalaTest {
    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chiseltest:0.6.0"
    )
  }
}
