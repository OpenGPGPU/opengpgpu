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

case object ThreadNum extends Field[Int]
case object XLen extends Field[Int]
case object WarpNum extends Field[Int]
case object RegNum extends Field[Int]
case object RegIDWidth extends Field[Int]
case object WarpIDWidth extends Field[Int]
case object AddrWidth extends Field[Int]
case object CacheBlockBytes extends Field[Int](64)
case object ExtMem extends Field[Option[MemoryPortParams]](None)
case object MemoryBusKey extends Field[MemoryBusParams]
case object SystemBusKey extends Field[SystemBusParams]

class CoreConfig
    extends Config((site, here, up) => {
      case ThreadNum       => 32
      case XLen            => 32
      case WarpNum         => 8
      case RegNum          => 32
      case RegIDWidth      => log2Ceil(site(RegNum))
      case WarpIDWidth     => log2Ceil(site(WarpNum))
      case AddrWidth       => 32
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
