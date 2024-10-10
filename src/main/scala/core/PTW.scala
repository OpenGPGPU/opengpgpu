package ogpu.core

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{M_XRD, PRV}

class PTE() extends Bundle {
  val reserved_for_future = UInt(10.W)
  val ppn = UInt(44.W)
  val reserved_for_software = Bits(2.W)

  /** dirty bit */
  val d = Bool()

  /** access bit */
  val a = Bool()

  /** global mapping */
  val g = Bool()

  /** user mode accessible */
  val u = Bool()

  /** whether the page is executable */
  val x = Bool()

  /** whether the page is writable */
  val w = Bool()

  /** whether the page is readable */
  val r = Bool()

  /** valid bit */
  val v = Bool()

  /** return true if find a pointer to next level page table */
  def table(dummy: Int = 0) = v && !r && !w && !x && !d && !a && !u && reserved_for_future === 0.U

  /** return true if find a leaf PTE */
  def leaf(dummy: Int = 0) = v && (r || (x && !w)) && a

  /** user read */
  def ur(dummy: Int = 0) = sr() && u

  /** user write */
  def uw(dummy: Int = 0) = sw() && u

  /** user execute */
  def ux(dummy: Int = 0) = sx() && u

  /** supervisor read */
  def sr(dummy: Int = 0) = leaf() && r

  /** supervisor write */
  def sw(dummy: Int = 0) = leaf() && w && d

  /** supervisor execute */
  def sx(dummy: Int = 0) = leaf() && x

  /** full permission: writable and executable in user mode */
  def isFullPerm(dummy: Int = 0) = uw() && ux()
}

class PTWReq(vpnBits: Int) extends Bundle {
  val addr = UInt(vpnBits.W)
  val vstage1 = Bool()
  val stage2 = Bool()
}

class PTWResp(vaddrBits: Int, pgLevels: Int) extends Bundle {

  /** ptw access exception */
  val ae_ptw = Bool()

  /** final access exception */
  val ae_final = Bool()

  /** page fault */
  val pf = Bool()

  // /** guest page fault */
  // val gf = Bool()

  // /** hypervisor read */
  // val hr = Bool()

  // /** hypervisor write */
  // val hw = Bool()

  // /** hypervisor execute */
  // val hx = Bool()

  /** PTE to refill L1TLB
    *
    * source: L2TLB
    */
  val pte = new PTE

  /** pte pglevel */
  val level = UInt(log2Ceil(pgLevels).W)

  /** fragmented_superpage support */
  // val fragmented_superpage = Bool()

  /** homogeneous for both pma and pmp */
  val homogeneous = Bool()
  // val gpa = Valid(UInt(vaddrBits.W))
  // val gpa_is_pte = Bool()
}

class PTBR() extends Bundle {
  val mode = UInt(4.W)
  val asid = UInt(16.W)
  val ppn = UInt(44.W)
}

/** IO between TLB and PTW
  *
  * PTW receives :
  *   - PTE request
  *   - CSRs info
  *   - pmp results from PMP(in TLB)
  */
class TLBPTWIO(vpnBits: Int, vaddrBits: Int, pgLevels: Int) extends Bundle {
  val req = Decoupled(Valid(new PTWReq(vpnBits)))
  val resp = Flipped(Valid(new PTWResp(vaddrBits, pgLevels)))
  val ptbr = Input(new PTBR())

  // val hgatp = Input(new PTBR())
  // val vsatp = Input(new PTBR())
  val status = Input(new MStatus())
  // val hstatus = Input(new HStatus())
  // val gstatus = Input(new MStatus())
  // val customCSRs = Flipped(coreParams.customCSRs)
}

class DatapathPTWIO(vaddrBits: Int) extends Bundle {
  val ptbr = Input(new PTBR())
  val sfence = Flipped(Valid(new SFenceReq(vaddrBits)))
  val status = Input(new MStatus())
  val clock_enabled = Output(Bool())
}

case class PTWParameter(
  paddrBits:   Int,
  vaddrBits:   Int,
  pgIdxBits:   Int = 12,
  pgLevelBits: Int = 9,
  nSectors:    Int = 4,
  xLen:        Int = 64,
  pgLevels:    Int = 3) {

  def ppnBits: Int = paddrBits - pgIdxBits
  def vpnBits: Int = vaddrBits - pgIdxBits
}

/** PTW contains L2TLB, and performs page table walk for high level TLB, and cache queries from L1 TLBs(I$, D$, RoCC)
  *
  * It performs hierarchy page table query to mem for the desired leaf PTE and cache them in l2tlb. Besides leaf PTEs,
  * it also caches non-leaf PTEs in pte_cache to accerlerate the process.
  *
  * ==Structure==
  *   - l2tlb : for leaf PTEs
  *     - set-associative (configurable with [[CoreParams.nL2TLBEntries]]and [[CoreParams.nL2TLBWays]]))
  *     - PLRU
  *   - pte_cache: for non-leaf PTEs
  *     - set-associative
  *     - LRU
  *   - s2_pte_cache: for non-leaf PTEs in 2-stage translation
  *     - set-associative
  *     - PLRU
  *
  * l2tlb Pipeline: 3 stage
  * {{{
  * stage 0 : read
  * stage 1 : decode
  * stage 2 : hit check
  * }}}
  * ==State Machine==
  * s_ready: ready to reveive request from TLB s_req: request mem; pte_cache hit judge s_wait1: deal with l2tlb error
  * s_wait2: final hit judge s_wait3: receive mem response s_fragment_superpage: for superpage PTE
  *
  * @note
  *   l2tlb hit happens in s_req or s_wait1
  * @see
  *   RV-priv spec 4.3-4.6 for Virtual-Memory System
  * @see
  *   RV-priv spec 8.5 for Two-Stage Address Translation
  * @todo
  *   details in two-stage translation
  */
class PTW(n: Int, cfg: PTWParameter, cache_cfg: CacheParameter) extends Module {
  val io = IO(new Bundle {

    /** to n TLB */
    val requestor = Flipped(Vec(n, new TLBPTWIO(cfg.vpnBits, cfg.vaddrBits, cfg.pgLevels)))

    /** to Cache */
    val mem = new CacheIO(cache_cfg)

    /** to Core
      *
      * contains CSRs info and performance statistics
      */
    val dpath = new DatapathPTWIO(cfg.vaddrBits)
  })

  val s_ready :: s_req :: s_wait1 :: s_dummy1 :: s_wait2 :: s_wait3 :: s_dummy2 :: s_fragment_superpage :: Nil = Enum(8)
  val state = RegInit(s_ready)
  val l2_refill_wire = Wire(Bool())

  /** Arbiter to arbite request from nTLB */
  val arb = Module(new Arbiter(Valid(new PTWReq(cfg.vpnBits)), n))
  // use TLB req as arbitor's input
  arb.io.in <> io.requestor.map(_.req)
  // receive req only when s_ready and not in refill
  arb.io.out.ready := (state === s_ready) && !l2_refill_wire

  val resp_valid = RegNext(VecInit(Seq.fill(io.requestor.size)(false.B)))

  val clock_en =
    state =/= s_ready || l2_refill_wire || arb.io.out.valid || io.dpath.sfence.valid
  io.dpath.clock_enabled := clock_en

  val invalidated = Reg(Bool())

  /** current PTE level
    * {{{
    * 0 <= count <= pgLevel-1
    * count = pgLevel - 1 : leaf PTE
    * count < pgLevel - 1 : non-leaf PTE
    * }}}
    */
  val count = Reg(UInt(log2Ceil(cfg.pgLevels).W))
  val resp_ae_ptw = Reg(Bool())
  val resp_ae_final = Reg(Bool())
  val resp_pf = Reg(Bool())

  /** tlb request */
  val r_req = Reg(new PTWReq(cfg.vpnBits))

  /** current selected way in arbitor */
  val r_req_dest = Reg(Bits())
  // to construct mem.req.addr
  val r_pte = Reg(new PTE)

  val aux_pte = Reg(new PTE)

  val satp = io.dpath.ptbr
  val vpn = r_req.addr

  val mem_resp_valid = RegNext(io.mem.resp.valid)
  val mem_resp_data = RegNext(io.mem.resp.bits.data)
  // io.mem.uncached_resp.map { resp =>
  //   assert(!(resp.valid && io.mem.resp.valid))
  //   resp.ready := true.B
  //   when(resp.valid) {
  //     mem_resp_valid := true.B
  //     mem_resp_data := resp.bits.data
  //   }
  // }
  // construct pte from mem.resp
  val (pte, invalid_paddr) = {
    val tmp = mem_resp_data.asTypeOf(new PTE())
    val res = WireDefault(tmp)
    res.ppn := tmp.ppn(cfg.ppnBits - 1, 0)
    when(tmp.r || tmp.w || tmp.x) {
      // for superpage mappings, make sure PPN LSBs are zero
      for (i <- 0 until cfg.pgLevels - 1)
        when(
          count <= i.U && tmp.ppn(
            (cfg.pgLevels - 1 - i) * cfg.pgLevelBits - 1,
            (cfg.pgLevels - 2 - i) * cfg.pgLevelBits
          ) =/= 0.U
        ) { res.v := false.B }
    }
    (res, (tmp.ppn >> cfg.ppnBits) =/= 0.U)
  }
  // find non-leaf PTE, need traverse
  val traverse = pte.table() && !invalid_paddr && count < (cfg.pgLevels - 1).U

  /** address send to mem for enquerry */
  val pte_addr = {
    val vpn_idxs = (0 until cfg.pgLevels).map { i =>
      val width = cfg.pgLevelBits
      (vpn >> (cfg.pgLevels - i - 1) * cfg.pgLevelBits)(width - 1, 0)
    }
    val mask = ((1 << cfg.pgLevelBits) - 1).U
    val vpn_idx = vpn_idxs(count) & mask
    val raw_pte_addr = ((r_pte.ppn << cfg.pgLevelBits) | vpn_idx) << log2Ceil(cfg.xLen / 8)
    val size = cfg.paddrBits
    // use r_pte.ppn as page table base address
    // use vpn slice as offset
    raw_pte_addr.apply(size.min(raw_pte_addr.getWidth) - 1, 0)
  }

  /** pte_cache input addr */
  val pte_cache_addr = pte_addr

  /** PTECache caches non-leaf PTE
    * @param s2
    *   true: 2-stage address translation
    */
  def makePTECache(s2: Boolean): (Bool, UInt) = (false.B, 0.U)
  // generate pte_cache
  val (pte_cache_hit, pte_cache_data) = makePTECache(false)
  // pte_cache hit or 2-stage pte_cache hit
  val pte_hit = RegNext(false.B)
  // l2_refill happens when find the leaf pte
  val l2_refill = RegNext(false.B)
  l2_refill_wire := l2_refill
  // l2tlb
  val (l2_hit, l2_error, l2_pte, l2_tlb_ram) = (false.B, false.B, WireDefault(0.U.asTypeOf(new PTE)), None)

  // if SFENCE occurs during walk, don't refill PTE cache or L2 TLB until next walk
  invalidated := io.dpath.sfence.valid || (invalidated && state =/= s_ready)
  // mem request
  // io.mem.keep_clock_enabled := false.B

  io.mem.req.valid := state === s_req
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.phys := true.B
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.size := log2Ceil(cfg.xLen / 8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.addr := pte_addr
  // io.mem.req.bits.idx.foreach(_ := pte_addr)
  io.mem.req.bits.dprv := PRV.S.U // PTW accesses are S-mode by definition
  io.mem.req.bits.dv := false.B
  // io.mem.req.bits.tag := DontCare
  io.mem.req.bits.no_alloc := DontCare
  io.mem.req.bits.no_xcpt := DontCare
  io.mem.req.bits.data := DontCare
  io.mem.req.bits.mask := DontCare

  io.mem.s1_kill := l2_hit || state =/= s_wait1
  io.mem.s1_data := DontCare
  io.mem.s2_kill := false.B

  val homogeneous = true.B
  // response to tlb
  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_valid(i)
    io.requestor(i).resp.bits.ae_ptw := resp_ae_ptw
    io.requestor(i).resp.bits.ae_final := resp_ae_final
    io.requestor(i).resp.bits.pf := resp_pf
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.level := count
    io.requestor(i).resp.bits.homogeneous := homogeneous
    io.requestor(i).ptbr := io.dpath.ptbr
    // io.requestor(i).customCSRs <> io.dpath.customCSRs
    io.requestor(i).status := io.dpath.status
    // io.requestor(i).pmp := io.dpath.pmp
  }

  // control state machine
  val next_state = WireDefault(state)
  state := OptimizationBarrier(next_state)

  switch(state) {
    is(s_ready) {
      when(arb.io.out.fire) {
        val aux_ppn = arb.io.out.bits.bits.addr

        r_req := arb.io.out.bits.bits
        r_req_dest := arb.io.chosen
        next_state := Mux(arb.io.out.bits.valid, s_req, s_ready)
        count := 0.U
        aux_pte.ppn := aux_ppn
        aux_pte.reserved_for_future := 0.U
        resp_ae_ptw := false.B
        resp_ae_final := false.B
      }
    }
    is(s_req) {
      // pte_cache hit
      when(pte_cache_hit) {
        count := count + 1.U
        pte_hit := true.B
      }.otherwise {
        next_state := Mux(io.mem.req.ready, s_wait1, s_req)
      }
    }
    is(s_wait1) {
      // This Mux is for the l2_error case; the l2_hit && !l2_error case is overriden below
      next_state := Mux(l2_hit, s_req, s_wait2)
    }
    is(s_wait2) {
      next_state := s_wait3
      when(io.mem.s2_xcpt.ae.ld) {
        resp_ae_ptw := true.B
        next_state := s_ready
        resp_valid(r_req_dest) := true.B
      }
    }
  }

  r_pte := OptimizationBarrier(
    // l2tlb hit->find a leaf PTE(l2_pte), respond to L1TLB
    Mux(
      l2_hit && !l2_error,
      l2_pte,
      // pte cache hit->find a non-leaf PTE(pte_cache),continue to request mem
      Mux(
        state === s_req && pte_cache_hit,
        makePTE(pte_cache_data, l2_pte),
        // when mem respond, store mem.resp.pte
        Mux(
          mem_resp_valid,
          pte,
          // when tlb request come->request mem, use root address in satp(or vsatp,hgatp)
          Mux(arb.io.out.fire, makePTE(satp.ppn, r_pte), r_pte)
        )
      )
    )
  )

  when(l2_hit && !l2_error) {
    assert(state === s_req || state === s_wait1)
    next_state := s_ready
    resp_valid(r_req_dest) := true.B
    count := (cfg.pgLevels - 1).U
  }
  when(mem_resp_valid) {
    assert(state === s_wait3)
    next_state := s_req
    when(traverse) {
      count := count + 1.U
    }.otherwise {
      val ae = pte.v && invalid_paddr
      val pf = pte.v && pte.reserved_for_future =/= 0.U
      val success = pte.v && !ae && !pf

      // find a leaf pte, start l2 refill
      l2_refill := success && count === (cfg.pgLevels - 1).U
      count := 0.U

      next_state := s_ready
      resp_valid(r_req_dest) := true.B

      resp_ae_ptw := ae && count < (cfg.pgLevels - 1).U && pte.table()
      resp_ae_final := ae
      resp_pf := pf
    }
  }
  when(io.mem.s2_nack) {
    assert(state === s_wait2)
    next_state := s_req
  }

  /** Relace PTE.ppn with ppn */
  private def makePTE(ppn: UInt, default: PTE) = {
    val pte = WireDefault(default)
    pte.ppn := ppn
    pte
  }
}
