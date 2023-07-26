/*
 * Copyright (c) 2023 OpenGPGPU
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package opengpgpu.config

import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem.{MemoryBusParams, SystemBusParams}

/** Specifies the size and width of external memory ports */
case class MasterPortParams(
    base: BigInt,
    size: BigInt,
    beatBytes: Int,
    idBits: Int,
    maxXferBytes: Int = 256,
    executable: Boolean = true
)

/** Specifies the width of external slave ports */
case class SlavePortParams(beatBytes: Int, idBits: Int, sourceBits: Int)
// if incohBase is set, creates an incoherent alias for the region that hangs off the sbus
case class MemoryPortParams(master: MasterPortParams, nMemoryChannels: Int, incohBase: Option[BigInt] = None)

case object ThreadNum       extends Field[Int]
case object XLen            extends Field[Int]
case object CacheBlockBytes extends Field[Int](64)
case object ExtMem          extends Field[Option[MemoryPortParams]](None)
case object MemoryBusKey    extends Field[MemoryBusParams]
case object SystemBusKey    extends Field[SystemBusParams]

class CoreConfig
    extends Config((site, here, up) => {
      case ThreadNum       => 32
      case XLen            => 32
      case CacheBlockBytes => 4
      case MemoryBusKey    => MemoryBusParams(beatBytes = site(XLen) / 8, blockBytes = site(CacheBlockBytes))
      case SystemBusKey    => SystemBusParams(beatBytes = site(XLen) / 8, blockBytes = site(CacheBlockBytes))

      case ExtMem =>
        Some(
          MemoryPortParams(
            MasterPortParams(
              base = 0x0000_0000,
              size = 0x1000_0000,
              beatBytes = site(MemoryBusKey).beatBytes,
              idBits = 4
            ),
            1
          )
        )
    })

object parameters {
  def xLen                  = 32 // data length 32-bit
  def NUMBER_THREAD         = 32
  var NUMBER_CU             = 4
  var NUMBER_WARP           = 8
  var NUMBER_RES_TABLE      = 2
  var ADDR_WIDTH            = 32
  var NUMBER_IREG           = 32
  var REG_ID_WIDTH          = log2Ceil(NUMBER_IREG)
  var NUMBER_VGPR_SLOTS     = 4096
  var NUMBER_SGPR_SLOTS     = 4096
  var NUMBER_LDS_SLOTS      = 4096
  var WG_ID_WIDTH           = 15 // Format: wg id + prefer scheduler (if multi-schedulers) + prefer cu
  var NUMBER_WF_SLOTS       = 8
  var WF_COUNT_MAX          = 256
  var WF_COUNT_PER_WG_MAX   = 32
  var GDS_SIZE              = 1024
  var NUMBER_ENTRIES        = 2  // This parameter should be a power of 2
  var WAVE_ITEM_WIDTH       = 10
  var MEM_ADDR_WIDTH        = 32
  var NUM_SCHEDULER         = 2
  var CU_ID_WIDTH           = Math.max(log2Ceil(NUMBER_CU), 1)
  var RES_TABLE_ADDR_WIDTH  = Math.max(log2Ceil(NUMBER_RES_TABLE), 1)
  var VGPR_ID_WIDTH         = log2Ceil(NUMBER_VGPR_SLOTS)
  var SGPR_ID_WIDTH         = log2Ceil(NUMBER_SGPR_SLOTS)
  var LDS_ID_WIDTH          = log2Ceil(NUMBER_LDS_SLOTS)
  var WG_SLOT_ID_WIDTH      = log2Ceil(NUMBER_WF_SLOTS)
  var WF_COUNT_WIDTH        = log2Ceil(WF_COUNT_MAX) + 1
  var WF_COUNT_WIDTH_PER_WG = log2Ceil(WF_COUNT_PER_WG_MAX) + 1
  var GDS_ID_WIDTH          = log2Ceil(GDS_SIZE)
  var ENTRY_ADDR_WIDTH      = log2Ceil(NUMBER_ENTRIES)
  var TAG_WIDTH             = WG_SLOT_ID_WIDTH + WF_COUNT_WIDTH_PER_WG
  var INIT_MAX_WG_COUNT     = NUMBER_WF_SLOTS
  var NUM_SCHEDULER_WIDTH   = Math.max(log2Ceil(NUM_SCHEDULER), 1)
}
