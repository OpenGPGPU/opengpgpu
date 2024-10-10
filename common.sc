import mill._
import mill.scalalib._

trait OGPUModule extends ScalaModule {

  def rocketModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
  )
}
