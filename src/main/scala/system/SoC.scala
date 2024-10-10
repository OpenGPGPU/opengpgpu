package ogpu.system

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._

case object SoCParamsKey extends Field[SoCParameters]

/** Global cache coherence granularity, which applies to all caches, for now. */
// case object CacheBlockBytes extends Field[Int](64)

case class SoCParameters(
  EnableILA: Boolean = false,
  PAddrBits: Int = 36,
  extIntrs:  Int = 64) {
  // L3 configurations
  val L3InnerBusWidth = 256
  val L3BlockSize = 64
  // on chip network configurations
  val L3OuterBusWidth = 256
}

trait HasSoCParameter {
  implicit val p: Parameters

  val soc = p(SoCParamsKey)
  // val debugOpts = p(DebugOptionsKey)
  // val tiles = p(XSTileKey)

  // val NumCores = tiles.size
  val EnableILA = soc.EnableILA

  // L3 configurations
  val L3InnerBusWidth = soc.L3InnerBusWidth
  val L3BlockSize = soc.L3BlockSize

  // on chip network configurations
  val L3OuterBusWidth = soc.L3OuterBusWidth

  val NrExtIntr = soc.extIntrs
}

abstract class OGPUSystem(
  implicit p: Parameters)
    extends LazyModule {}
