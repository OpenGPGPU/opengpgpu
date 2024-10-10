package ogpu.smmu

import chisel3._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

case class SMMUParams(baseAddress: BigInt = 0x02000000) {
  def address = AddressSet(baseAddress, 0xff)
}

class TLSMMU(
  params: SMMUParams
)(
  implicit p: Parameters)
    extends LazyModule {

  val device = new SimpleDevice("smmu", Seq("ogpu, smmu0")) {
    override val alwaysExtended = true
  }

  val node = TLRegisterNode(address = Seq(params.address), device = device, beatBytes = 8)
  // val ptwlm = LazyModule(new L2TLBWrapper())

  lazy val module = new Impl(this)
  class Impl(
    outer: TLSMMU
  )(
    implicit p: Parameters)
      extends LazyModuleImp(outer) {
    val io = IO(new Bundle {
      // val tlb = Flipped(new TlbRequestIO(1))
    })

    // val (mem, edge) = outer.ptwlm.node.out.head
    // val satp = RegInit(0.U(64.W))

    // val ptwm = ptwlm.module
    // val tlbm = Module(new TLB(1, nRespDups = 1, Seq(true), new TLBParameters))

    // val tlb_ptw = Wire(new VectorTlbPtwIO(1))
    // tlb_ptw.connect(tlbm.io.ptw)

    // val sfence = WireInit(0.U.asTypeOf(new SfenceBundle))
    // val tlbCsr = WireInit(0.U.asTypeOf(new TlbCsrBundle))
    // tlbCsr.satp.apply(satp)

    // tlbm.io.requestor(0) <> io.tlb
    // tlbm.io.csr := tlbCsr
    // tlbm.io.sfence := sfence
    // tlbm.io.hartId := 0.U
    // tlbm.io.flushPipe := 0.U.asTypeOf(tlbm.io.flushPipe)

    // val tlbRepeater1 = PTWFilter(16, tlb_ptw, sfence, tlbCsr, 8)
    // val tlbRepeater2 = PTWRepeaterNB(passReady = false, 16, tlbRepeater1.io.ptw, ptwm.io.tlb(0), sfence, tlbCsr)

    // ptwm.io.csr.tlb.satp.apply(satp)
    // ptwm.io.csr.tlb.priv := 0.U.asTypeOf(ptwm.io.csr.tlb.priv)

    // ptwm.io.sfence := sfence
    // ptwm.io.tlb(1) <> 0.U.asTypeOf(ptwm.io.tlb(1))

    // ptwm.io.hartId := 0.U

    // // CSR has been written by csr inst, copies of csr should be updated
    // // for pmp, we dont use it
    // ptwm.io.csr.distribute_csr := 0.U.asTypeOf(ptwm.io.csr.distribute_csr)

    // tlbRepeater1.io.debugTopDown := DontCare

    // // 0 satp.ppn  sv39 and sv48
    // //   bits 63:60  mode
    // //   bits 59:44  asid
    // //   bits 43:0   ppn
    // node.regmap(
    //   0 -> Seq(RegField(64, satp, RegFieldDesc("satp", "satp: SMMU satp rw register.", reset = Some(0))))
    // )
  }
}
