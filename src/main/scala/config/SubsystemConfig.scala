// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package ogpu.config

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

class BaseSubsystemConfig extends Config((_, _, _) => { case XLen => 64 })

class WithDTS(model: String, compat: Seq[String])
    extends Config((_, _, _) => {
      case DTSModel  => model
      case DTSCompat => compat
    })
