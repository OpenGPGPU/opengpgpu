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


package opengpgpu.subsystem

import chisel3._

import org.chipsalliance.cde.config.Field
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem._


/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait AXI4MemPort { this: AXI4Master =>
  private val memPortParamsOpt = p(ExtMem)
  private val portName = "axi4"
  private val device = new MemoryDevice
  private val idBits = memPortParamsOpt.map(_.master.idBits).getOrElse(1)

  // hardcode
  private val mbus = new MemoryBus(MemoryBusParams(4, 4))
  private val sbus = new SystemBus(SystemBusParams(4, 4))

  val memAXI4Node = AXI4SlaveNode(memPortParamsOpt.map({ case MemoryPortParams(memPortParams, nMemoryChannels, _) =>
    Seq.tabulate(nMemoryChannels) { channel =>
      val base = AddressSet.misaligned(memPortParams.base, memPortParams.size)
      val filter = AddressSet(channel * mbus.blockBytes, ~((nMemoryChannels-1) * mbus.blockBytes))

      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = base.flatMap(_.intersect(filter)),
          resources     = device.reg,
          regionType    = RegionType.UNCACHED, // cacheable
          executable    = true,
          supportsWrite = TransferSizes(1, mbus.blockBytes),
          supportsRead  = TransferSizes(1, mbus.blockBytes),
          interleavedId = Some(0))), // slave does not interleave read responses
        beatBytes = memPortParams.beatBytes)
    }
  }).toList.flatten)

  for (i <- 0 until memAXI4Node.portParams.size) {
    val mem_bypass_xbar = mbus { TLXbar() }

    // Create an incoherent alias for the AXI4 memory
    memPortParamsOpt.foreach(memPortParams => {
      memPortParams.incohBase.foreach(incohBase => {
        val cohRegion = AddressSet(0, incohBase-1)
        val incohRegion = AddressSet(incohBase, incohBase-1)
        val replicator = sbus {
          val replicator = LazyModule(new RegionReplicator(ReplicatedRegion(cohRegion, cohRegion.widen(incohBase))))
          val prefixSource = BundleBridgeSource[UInt](() => UInt(1.W))
          replicator.prefix := prefixSource
          // prefix is unused for TL uncached, so this is ok
          InModuleBody { prefixSource.bundle := 0.U(1.W) }
          replicator
        }
        sbus.coupleTo(s"memory_controller_bypass_port_named_$portName") {
          (mbus.crossIn(mem_bypass_xbar)(ValName("bus_xing"))(p(SbusToMbusXTypeKey))
            := TLWidthWidget(sbus.beatBytes)
            := replicator.node
            := TLFilter(TLFilter.mSubtract(cohRegion))
            := TLFilter(TLFilter.mResourceRemover)
            := _
          )
        }
      })
    })

    mbus.coupleTo(s"memory_controller_port_named_$portName") {
      (memAXI4Node
        := AXI4UserYanker()
        := AXI4IdIndexer(idBits)
        := TLToAXI4()
        := TLWidthWidget(mbus.beatBytes)
        := mem_bypass_xbar
        := _
      )
    }
  }

  val mem_axi4 = InModuleBody { memAXI4Node.makeIOs() }
}

