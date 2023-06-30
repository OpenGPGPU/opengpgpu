import mill._, scalalib._

import mill.scalalib.TestModule.ScalaTest

object opengpgpu extends SbtModule { m =>
  override def millSourcePath = os.pwd
  def scalaVersion = "2.13.8"

  def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.4"
  )
  
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.5.4",
  )

  object test extends Tests with ScalaTest {
    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chiseltest:0.5.4"
    )
  }
}
