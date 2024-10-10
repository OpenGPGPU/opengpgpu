// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package ogpu.config

import chisel3.util._
import org.chipsalliance.cde.config._

class OGPUBaseConfig(n: Int)
    extends Config((site, _, _) => {
      case DTSModel    => ""
      case ThreadNum   => 32
      case XLen        => 64
      case WarpNum     => 8
      case RegNum      => 32
      case RegIDWidth  => log2Ceil(site(RegNum))
      case WarpIDWidth => log2Ceil(site(WarpNum))
      case AddrWidth   => 64
      case StackDepth  => 16
      case DimWidth    => 16
    })

class OGPUDefaultConfig(n: Int = 1)
    extends Config(
      new OGPUBaseConfig(n)
    )
