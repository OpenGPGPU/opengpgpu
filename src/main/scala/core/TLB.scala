package ogpu.core

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo
import freechips.rocketchip.rocket.{
  isAMOArithmetic,
  isAMOLogical,
  isRead,
  isWrite,
  M_FLUSH_ALL,
  M_PWR,
  M_SZ,
  M_WOK,
  M_XLR,
  M_XSC,
  PRV
}
import freechips.rocketchip.util._

case class TLBParameter(
  nSets:       Int,
  nWays:       Int,
  paddrBits:   Int,
  vaddrBits:   Int,
  xLen:        Int = 32,
  pgIdxBits:   Int = 12,
  minPgLevels: Int = 2,
  pgLevelBits: Int = 9,
  nSectors:    Int = 4,
  pgLevels:    Int = 3) {

  def ppnBits: Int = paddrBits - pgIdxBits
  def vpnBits: Int = vaddrBits - pgIdxBits

}

class SFenceReq(vaddrBits: Int) extends Bundle {
  val rs1 = Bool()
  val rs2 = Bool()
  val addr = UInt(vaddrBits.W)
  val asid = UInt(16.W)
}

class TLBReq(lgMaxSize: Int, vaddrBits: Int) extends Bundle {

  /** request address from CPU. */
  val vaddr = UInt(vaddrBits.W)

  /** don't lookup TLB, bypass vaddr as paddr */
  val passthrough = Bool()

  /** granularity */
  val size = UInt(log2Ceil(lgMaxSize + 1).W)

  /** memory command. */
  val cmd = Bits(M_SZ.W)
  val prv = UInt(PRV.SZ.W)

  /** virtualization mode */
  // val v = Bool()

}

class TLBExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
  val inst = Bool()
}

class TLBResp(paddrBits: Int, vaddrBits: Int) extends Bundle {
  // lookup responses
  val miss = Bool()

  /** physical address */
  val paddr = UInt(paddrBits.W)

  /** page fault exception */
  val pf = new TLBExceptions

  /** access exception */
  val ae = new TLBExceptions

  /** misaligned access exception */
  val ma = new TLBExceptions
}

class TLBEntryData(tlbParam: TLBParameter) extends Bundle {
  val ppn = UInt(tlbParam.ppnBits.W)

  /** pte.u user */
  val u = Bool()

  /** access exception. D$ -> PTW -> TLB AE Alignment failed.
    */
  val ae_ptw = Bool()
  val ae_final = Bool()

  /** page fault */
  val pf = Bool()

  /** prot_w */
  val pw = Bool()

  /** prot_x */
  val px = Bool()

  /** prot_r */
  val pr = Bool()

}

/** basic cell for TLB data */
class TLBEntry(val tlbParam: TLBParameter) extends Bundle {

  val level = UInt(log2Ceil(tlbParam.pgLevels).W)

  /** use vpn as tag */
  val tag_vpn = UInt(tlbParam.vpnBits.W)

  val tag_asid = Vec(tlbParam.nSectors, UInt(16.W))

  /** entry data */
  val data = Vec(tlbParam.nSectors, UInt(new TLBEntryData(tlbParam).getWidth.W))

  /** valid bit */
  val valid = Vec(tlbParam.nSectors, Bool())

  /** returns all entry data in this entry */
  def entry_data = data.map(_.asTypeOf(new TLBEntryData(tlbParam)))

  /** returns the index of sector */
  private def sectorIdx(vpn: UInt) = vpn.extract(tlbParam.nSectors.log2 - 1, 0)

  /** returns the entry data matched with this vpn */
  def getData(vpn: UInt) = OptimizationBarrier(data(sectorIdx(vpn)).asTypeOf(new TLBEntryData(tlbParam)))

  /** returns whether a sector hits */
  def sectorHit(vpn: UInt) = valid.orR && sectorTagMatch(vpn)

  /** returns whether tag matches vpn */
  def sectorTagMatch(vpn: UInt) =
    (((tag_vpn ^ vpn) >> tlbParam.nSectors.log2) === 0.U)

  /** returns hit signal */
  def hit(vpn: UInt, asid: UInt): Bool = {
    val idx = sectorIdx(vpn)
    (tag_asid(idx) === asid) && valid(idx) && sectorTagMatch(vpn)
  }

  /** returns the ppn of the input TLBEntryData */
  def ppn(data: TLBEntryData) = {
    data.ppn
  }

  /** does the refill
    *
    * find the target entry with vpn tag and replace the target entry with the input entry data
    */
  def insert(vpn: UInt, asid: UInt, level: UInt, entry: TLBEntryData): Unit = {
    this.tag_vpn := vpn
    this.level := level.extract(log2Ceil(tlbParam.pgLevels) - 1, 0)

    val idx = sectorIdx(vpn)
    valid(idx) := true.B
    data(idx) := entry.asUInt
    tag_asid(idx) := asid
  }

  def invalidate(): Unit = { valid.foreach(_ := false.B) }
  def invalidate(asid: UInt): Unit = {
    for (((v, id), e) <- valid.zip(tag_asid).zip(entry_data))
      when(id === asid) { v := false.B }
  }
  def invalidateVPN(vpn: UInt, asid: UInt): Unit = {
    when(sectorTagMatch(vpn)) {
      for ((((v, id), e), i) <- (valid.zip(tag_asid).zip(entry_data)).zipWithIndex)
        when(id === asid && i.U === sectorIdx(vpn)) { v := false.B }
    }
  }
  def invalidateNonGlobal(asid: UInt): Unit = {
    for (((v, id), e) <- valid.zip(tag_asid).zip(entry_data))
      when(id === asid) { v := false.B }
  }
}

/** =Overview=
  * [[TLB]] is a TLB template.
  *
  * TLB caches PTE and accelerates the address translation process. When tlb miss happens, ask PTW(L2TLB) for Page Table
  * Walk.
  *
  * ==Cache Structure==
  *   - Sectored Entry (PTE)
  *     - set-associative or direct-mapped
  *       - nsets = [[nSets]]
  *       - nways = [[nWays]] / [[nSectors]]
  *       - PTEEntry( sectors = [[nSectors]] )
  *     - LRU(if set-associative)
  *
  * ==Address structure==
  * {{{
  * |vaddr                                                 |
  * |ppn/vpn                                   | pgIndex   |
  * |                                          |           |
  * |           |nSets             |nSector    |           |
  * }}}
  *
  * ==State Machine==
  * {{{
  * s_ready: ready to accept request from EXE.
  * s_request: when L1TLB(this) miss, send request to PTW(L2TLB), .
  * s_wait: wait for PTW to refill L1TLB.
  * s_wait_invalidate: L1TLB is waiting for respond from PTW, but L1TLB will invalidate respond from PTW.
  * }}}
  *
  * ==Note==
  * Boom use Rocket ITLB, and its own DTLB.
  *
  * Accelerators:{{{ sha3: DTLB gemmini: DTLB hwacha: DTLB*2+ITLB}}}
  * @param instruction
  *   true for ITLB, false for DTLB
  * @param lgMaxSize
  *   \@todo seems granularity
  * @param cfg
  *   [[TLBConfig]]
  * @param edge
  *   collect SoC metadata.
  */
class TLB(
  instruction: Boolean,
  cfg:         TLBParameter) //(
// implicit edge: TLEdgeOut,
// p:             Parameters)
    extends Module {
  val io = IO(new Bundle {

    /** request from Core */
    val req = Flipped(Decoupled(new TLBReq(cfg.xLen / 8, cfg.vaddrBits)))

    /** response to Core */
    val resp = Output(new TLBResp(cfg.paddrBits, cfg.vaddrBits))

    /** SFence Input */
    val sfence = Flipped(Valid(new SFenceReq(cfg.vaddrBits)))

    /** IO to PTW */
    val ptw = new TLBPTWIO(cfg.vpnBits, cfg.vaddrBits, cfg.pgLevels)

    /** suppress a TLB refill, one cycle after a miss */
    val kill = Input(Bool())
  })

  val usingAtomicsInCache = true
  val usingAtomics = true
  val vpn = io.req.bits.vaddr(cfg.vaddrBits - 1, cfg.pgIdxBits)

  /** index for sectored_Entry */
  val memIdx = vpn.extract(cfg.nSectors.log2 + cfg.nSets.log2 - 1, cfg.nSectors.log2)

  /** TLB Entry */
  val sectored_entries = Reg(Vec(cfg.nSets, Vec(cfg.nWays / cfg.nSectors, new TLBEntry(cfg))))
  def ordinary_entries = sectored_entries(memIdx)
  def all_entries = ordinary_entries
  def all_real_entries = sectored_entries.flatten

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(4)
  val state = RegInit(s_ready)
  // use vpn as refill_tag
  val r_refill_tag = Reg(UInt(cfg.vpnBits.W))
  val r_sectored_repl_addr = Reg(UInt(log2Ceil(sectored_entries.head.size).W))
  val r_sectored_hit = Reg(Valid(UInt(log2Ceil(sectored_entries.head.size).W)))

  /** privilege mode */
  val priv = io.req.bits.prv
  val priv_v = false.B
  val priv_s = priv(0)
  // user mode and supervisor mode
  val priv_uses_vm = priv <= PRV.S.U
  val satp = io.ptw.ptbr
  val asid = satp.asid
  val stage1_en = satp.mode(satp.mode.getWidth - 1)

  /** Enable Virtual Memory when:
    *   1. statically configured
    *   1. satp highest bits enabled
    *      i. RV32:
    *         - 0 -> Bare
    *         - 1 -> SV32
    *      i. RV64:
    *         - 0000 -> Bare
    *         - 1000 -> SV39
    *         - 1001 -> SV48
    *         - 1010 -> SV57
    *         - 1011 -> SV64
    *   1. In virtualization mode, vsatp highest bits enabled
    *   1. priv mode in U and S.
    *   1. in H & M mode, disable VM.
    *   1. no passthrough(micro-arch defined.)
    *
    * @see
    *   RV-priv spec 4.1.11 Supervisor Address Translation and Protection (satp) Register
    * @see
    *   RV-priv spec 8.2.18 Virtual Supervisor Address Translation and Protection Register (vsatp)
    */
  val vm_enabled = stage1_en && priv_uses_vm && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(cfg.ppnBits - 1, 0)

  /** refill signal */
  val do_refill = io.ptw.resp.valid

  /** sfence invalidate refill */
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate) || io.sfence.valid

  val mpu_ppn = refill_ppn
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(cfg.pgIdxBits - 1, 0))
  // PMA
  // check exist a slave can consume this address.
  // val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_ || _)
  // check utility to help check SoC property.
  // def fastCheck(member: TLManagerParameters => Boolean) =
  //   legal_address && edge.manager.fastProperty(mpu_physaddr, member, (b: Boolean) => b.B)

  // val cacheable = fastCheck(_.supportsAcquireB) && (instruction).B
  // val cacheable = (instruction).B

  val homogeneous = false.B
  //  TLBPageLookup(edge.manager.managers, cfg.xLen, cfg.CacheBlockBytes, BigInt(1) << cfg.pgIdxBits)(
  //    mpu_physaddr
  //  ).homogeneous
  val prot_r = true.B // fastCheck(_.supportsGet)
  val prot_w = true.B // fastCheck(_.supportsPutFull)
  val prot_pp = true.B // fastCheck(_.supportsPutPartial)
  val prot_al = true.B // fastCheck(_.supportsLogical)
  val prot_aa = true.B // fastCheck(_.supportsArithmetic)
  val prot_x = true.B // fastCheck(_.executable)
  val prot_eff = true.B // fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)

  // hit check
  val sector_hits = sectored_entries(memIdx).map(_.sectorHit(vpn))
  val hitsVec = all_entries.map(vm_enabled && _.hit(vpn, asid))
  val real_hits = hitsVec.asUInt
  val hits = Cat(!vm_enabled, real_hits)

  // use ptw response to refill
  // permission bit arrays
  when(do_refill) {
    val pte = io.ptw.resp.bits.pte
    // val refill_v = r_vstage1_en || r_stage2_en
    // val asid
    val newEntry = Wire(new TLBEntryData(cfg))
    newEntry.ppn := pte.ppn
    // newEntry.c := cacheable
    newEntry.u := pte.u
    // newEntry.g := pte.g && pte.v
    newEntry.ae_ptw := io.ptw.resp.bits.ae_ptw
    newEntry.ae_final := io.ptw.resp.bits.ae_final
    newEntry.pf := io.ptw.resp.bits.pf
    newEntry.pr := prot_r
    newEntry.pw := prot_w
    newEntry.px := prot_x
    // newEntry.ppp := prot_pp
    // newEntry.pal := prot_al
    // newEntry.paa := prot_aa
    // newEntry.eff := prot_eff
    // refill sectored_hit
    val r_memIdx = r_refill_tag.extract(cfg.nSectors.log2 + cfg.nSets.log2 - 1, cfg.nSectors.log2)
    val waddr = Mux(r_sectored_hit.valid, r_sectored_hit.bits, r_sectored_repl_addr)
    for ((e, i) <- sectored_entries(r_memIdx).zipWithIndex) when(waddr === i.U) {
      when(!r_sectored_hit.valid) { e.invalidate() }
      e.insert(r_refill_tag, asid, 0.U, newEntry)
      when(invalidate_refill) { e.invalidate() }
    }
  }

  // get all entries data.
  val entries = all_entries.map(_.getData(vpn))
  val normal_entries = entries.take(ordinary_entries.size)
  // parallel query PPN from [[all_entries]], if VM not enabled return VPN instead
  val ppn = Mux1H(
    hitsVec :+ !vm_enabled,
    (all_entries.zip(entries)).map { case (entry, data) => entry.ppn(data) } :+ vpn(cfg.ppnBits - 1, 0)
  )

  val nPhysicalEntries = 1
  // generally PTW misaligned load exception.
  val ptw_ae_array = Cat(false.B, entries.map(_.ae_ptw).asUInt)
  val final_ae_array = Cat(false.B, entries.map(_.ae_final).asUInt)
  val ptw_pf_array = Cat(false.B, entries.map(_.pf).asUInt)
  val sum = io.ptw.status.sum
  // if in hypervisor/machine mode, cannot read/write user entries.
  // if in superviosr/user mode, "If the SUM bit in the sstatus register is set, supervisor mode software may also access pages with U=1.(from spec)"
  val priv_rw_ok = entries.map(_.u).asUInt
  // if in hypervisor/machine mode, other than user pages, all pages are executable.
  // if in superviosr/user mode, only user page can execute.
  val priv_x_ok = entries.map(_.u).asUInt
  val mxr = io.ptw.status.mxr
  // "The vsstatus field MXR, which makes execute-only pages readable, only overrides VS-stage page protection.(from spec)"
  // val r_array =
  //   Cat(true.B, (priv_rw_ok & (entries.map(_.sr).asUInt | Mux(mxr, entries.map(_.sx).asUInt, 0.U))))
  // These array is for each TLB entries.
  // user mode can read: PMA OK, TLB OK, AE OK
  val pr_array = Cat(Fill(nPhysicalEntries, prot_r), normal_entries.map(_.pr).asUInt) & ~(ptw_ae_array | final_ae_array)
  // user mode can write: PMA OK, TLB OK, AE OK
  val pw_array = Cat(Fill(nPhysicalEntries, prot_w), normal_entries.map(_.pw).asUInt) & ~(ptw_ae_array | final_ae_array)
  // user mode can write: PMA OK, TLB OK, AE OK
  val px_array = Cat(Fill(nPhysicalEntries, prot_x), normal_entries.map(_.px).asUInt) & ~(ptw_ae_array | final_ae_array)
  // put effect
  // val eff_array = Cat(Fill(nPhysicalEntries, prot_eff), normal_entries.map(_.eff).asUInt)
  // cacheable
  // val c_array = Cat(Fill(nPhysicalEntries, cacheable), normal_entries.map(_.c).asUInt)
  // put partial
  // val ppp_array = Cat(Fill(nPhysicalEntries, prot_pp), normal_entries.map(_.ppp).asUInt)
  // // atomic arithmetic
  // val paa_array = Cat(Fill(nPhysicalEntries, prot_aa), normal_entries.map(_.paa).asUInt)
  // // atomic logic
  // val pal_array = Cat(Fill(nPhysicalEntries, prot_al), normal_entries.map(_.pal).asUInt)
  // val ppp_array_if_cached = ppp_array // | c_array
  // val paa_array_if_cached = paa_array // | (if (usingAtomicsInCache) c_array else 0.U)
  // val pal_array_if_cached = pal_array // | (if (usingAtomicsInCache) c_array else 0.U)

  // vaddr misaligned: vaddr[1:0]=b00
  val misaligned = (io.req.bits.vaddr & (UIntToOH(io.req.bits.size) - 1.U)).orR
  // def badVA(): Bool = {
  //   val additionalPgLevels = satp.additionalPgLevels
  //   val signed = 1
  //   val nPgLevelChoices = cfg.pgLevels - cfg.minPgLevels + 1
  //   val minVAddrBits = cfg.pgIdxBits + cfg.minPgLevels * cfg.pgLevelBits
  //   (for (i <- 0 until nPgLevelChoices) yield {
  //     val mask =
  //       ((BigInt(1) << cfg.vaddrBits) - (BigInt(1) << (minVAddrBits + i * cfg.pgLevelBits - signed.toInt))).U
  //     val maskedVAddr = io.req.bits.vaddr & mask
  //     additionalPgLevels === i.U && !(maskedVAddr === 0.U || signed.B && maskedVAddr === mask)
  //   }).orR
  // }
  val bad_gpa = false.B
  val bad_va = false.B

  val cmd_lrsc = usingAtomics.B && io.req.bits.cmd.isOneOf(M_XLR, M_XSC)
  val cmd_amo_logical = usingAtomics.B && isAMOLogical(io.req.bits.cmd)
  val cmd_amo_arithmetic = usingAtomics.B && isAMOArithmetic(io.req.bits.cmd)
  val cmd_put_partial = io.req.bits.cmd === M_PWR
  val cmd_read = isRead(io.req.bits.cmd)
  val cmd_readx = false.B
  val cmd_write = isWrite(io.req.bits.cmd)
  val cmd_write_perms = cmd_write ||
    io.req.bits.cmd.isOneOf(M_FLUSH_ALL, M_WOK) // not a write, but needs write permissions

  // val lrscAllowed = Mux((usingDataScratchpad || usingAtomicsOnlyForIO).B, 0.U, c_array)
  val lrscAllowed = 0.U
  val ae_array =
    // Mux(misaligned, eff_array, 0.U) |
    Mux(cmd_lrsc, ~lrscAllowed, 0.U)

  // access exception needs SoC information from PMA
  val ae_ld_array = Mux(cmd_read, ae_array | ~pr_array, 0.U)
  val ae_st_array =
    Mux(cmd_write_perms, ae_array | ~pw_array, 0.U) // |
  //  Mux(cmd_put_partial, ~ppp_array_if_cached, 0.U) |
  //  Mux(cmd_amo_logical, ~pal_array_if_cached, 0.U) |
  //  Mux(cmd_amo_arithmetic, ~paa_array_if_cached, 0.U)
  // val must_alloc_array =
  //   Mux(cmd_put_partial, ~ppp_array, 0.U) |
  //     Mux(cmd_amo_logical, ~pal_array, 0.U) |
  //     Mux(cmd_amo_arithmetic, ~paa_array, 0.U) |
  //     Mux(cmd_lrsc, ~0.U(pal_array.getWidth.W), 0.U)
  val pf_ld_array =
    Mux(cmd_read, (ptw_ae_array | ptw_pf_array), 0.U)
  val pf_st_array = Mux(cmd_write_perms, (ptw_ae_array | ptw_pf_array), 0.U)
  val pf_inst_array = (ptw_ae_array | ptw_pf_array)

  val tlb_hit_if_not_gpa_miss = real_hits.orR
  val tlb_hit = real_hits.orR
  // leads to s_request
  val tlb_miss = vm_enabled && !tlb_hit

  val sectored_plru = new SetAssocLRU(cfg.nSets, sectored_entries.head.size, "plru")
  when(io.req.valid && vm_enabled) {
    // replace
    when(sector_hits.orR) { sectored_plru.access(memIdx, OHToUInt(sector_hits)) }
  }

  // Superpages create the possibility that two entries in the TLB may match.
  // This corresponds to a software bug, but we can't return complete garbage;
  // we must return either the old translation or the new translation.  This
  // isn't compatible with the Mux1H approach.  So, flush the TLB and report
  // a miss on duplicate entries.
  val multipleHits = PopCountAtLeast(real_hits, 2)

  // only pull up req.ready when this is s_ready state.
  io.req.ready := state === s_ready
  // page fault
  io.resp.pf.ld := (bad_va && cmd_read) || (pf_ld_array & hits).orR
  io.resp.pf.st := (bad_va && cmd_write_perms) || (pf_st_array & hits).orR
  io.resp.pf.inst := bad_va || (pf_inst_array & hits).orR
  // access exception
  io.resp.ae.ld := (ae_ld_array & hits).orR
  io.resp.ae.st := (ae_st_array & hits).orR
  io.resp.ae.inst := (~px_array & hits).orR
  // misaligned
  io.resp.ma.ld := misaligned && cmd_read
  io.resp.ma.st := misaligned && cmd_write
  io.resp.ma.inst := false.B // this is up to the pipeline to figure out
  // io.resp.cacheable := (c_array & hits).orR
  // io.resp.must_alloc := (must_alloc_array & hits).orR
  // io.resp.prefetchable := (prefetchable_array & hits).orR // && edge.manager.managers
  // .forall(m => !m.supportsAcquireB || m.supportsHint)
  // .B
  io.resp.miss := do_refill || tlb_miss || multipleHits
  io.resp.paddr := Cat(ppn, io.req.bits.vaddr(cfg.pgIdxBits - 1, 0))

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.valid := !io.kill
  io.ptw.req.bits.bits.addr := r_refill_tag
  io.ptw.req.bits.bits.vstage1 := false.B
  io.ptw.req.bits.bits.stage2 := false.B

  val sfence = io.sfence.valid
  // this is [[s_ready]]
  // handle miss/hit at the first cycle.
  // if miss, request PTW(L2TLB).
  when(io.req.fire && tlb_miss) {
    state := s_request
    r_refill_tag := vpn
    r_sectored_repl_addr := replacementEntry(sectored_entries(memIdx), sectored_plru.way(memIdx))
    r_sectored_hit.valid := sector_hits.orR
    r_sectored_hit.bits := OHToUInt(sector_hits)
  }
  // Handle SFENCE.VMA when send request to PTW.
  // SFENCE.VMA    io.ptw.req.ready     kill
  //       ?                 ?            1
  //       0                 0            0
  //       0                 1            0 -> s_wait
  //       1                 0            0 -> s_wait_invalidate
  //       1                 0            0 -> s_ready
  when(state === s_request) {
    // SFENCE.VMA will kill TLB entries based on rs1 and rs2. It will take 1 cycle.
    when(sfence) { state := s_ready }
    // here should be io.ptw.req.fire, but assert(io.ptw.req.ready === true.B)
    // fire -> s_wait
    when(io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
    // If CPU kills request(frontend.s2_redirect)
    when(io.kill) { state := s_ready }
  }
  // sfence in refill will results in invalidate
  when(state === s_wait && sfence) {
    state := s_wait_invalidate
  }
  // after CPU acquire response, go back to s_ready.
  when(io.ptw.resp.valid) {
    state := s_ready
  }

  // SFENCE processing logic.
  when(sfence) {
    assert(!io.sfence.bits.rs1 || (io.sfence.bits.addr >> cfg.pgIdxBits) === vpn)
    for (e <- all_real_entries) {
      when(io.sfence.bits.rs1) { e.invalidateVPN(vpn, asid) }
        .elsewhen(io.sfence.bits.rs2) { e.invalidateNonGlobal(asid) }
        .otherwise { e.invalidate(asid) }
    }
  }

  when(multipleHits || reset.asBool) {
    all_real_entries.foreach(_.invalidate())
  }

  ccover(io.ptw.req.fire, "MISS", "TLB miss")
  ccover(io.ptw.req.valid && !io.ptw.req.ready, "PTW_STALL", "TLB miss, but PTW busy")
  ccover(state === s_wait_invalidate, "SFENCE_DURING_REFILL", "flush TLB during TLB refill")
  ccover(sfence && !io.sfence.bits.rs1 && !io.sfence.bits.rs2, "SFENCE_ALL", "flush TLB")
  ccover(sfence && !io.sfence.bits.rs1 && io.sfence.bits.rs2, "SFENCE_ASID", "flush TLB ASID")
  ccover(sfence && io.sfence.bits.rs1 && !io.sfence.bits.rs2, "SFENCE_LINE", "flush TLB line")
  ccover(sfence && io.sfence.bits.rs1 && io.sfence.bits.rs2, "SFENCE_LINE_ASID", "flush TLB line/ASID")
  ccover(multipleHits, "MULTIPLE_HITS", "Two matching translations in TLB")

  def ccover(
    cond:  Bool,
    label: String,
    desc:  String
  )(
    implicit sourceInfo: SourceInfo
  ) =
    property.cover(cond, s"${if (instruction) "I" else "D"}TLB_$label", "MemorySystem;;" + desc)

  /** Decides which entry to be replaced
    *
    * If there is a invalid entry, replace it with priorityencoder; if not, replace the alt entry
    *
    * @return
    *   mask for TLBEntry replacement
    */
  def replacementEntry(set: Seq[TLBEntry], alt: UInt) = {
    val valids = set.map(_.valid.orR).asUInt
    Mux(valids.andR, alt, PriorityEncoder(~valids))
  }
}
