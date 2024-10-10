package ogpu.core

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import chisel3.util.random.LFSR

case class ICacheParams(
  nSets:      Int = 64,
  nWays:      Int = 4,
  rowBits:    Int = 128,
  paddrBits:  Int = 48,
  vaddrBits:  Int = 48,
  pgIdxBits:  Int = 12,
  dataBits:   Int = 64,
  nTLBSets:   Int = 32,
  nTLBWays:   Int = 4,
  coreId:     Int = 0,
  tagECC:     Option[String] = None,
  dataECC:    Option[String] = None,
  prefetch:   Boolean = false,
  pgLevels:   Int = 3,
  blockBytes: Int = 64,
  latency:    Int = 2) {
  def tagCode:  Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = new RandomReplacement(nWays)
  def blockOffBits: Int = log2Ceil(blockBytes)
  def lgCacheBlockBytes = blockOffBits
  def untagBits:   Int = log2Ceil(nSets) + blockOffBits
  def tagBits:     Int = vaddrBits - untagBits
  def pgUntagBits: Int = untagBits
  def idxBits = log2Up(nSets)
  def isDM = nWays == 1
  def cacheDataBeats = (blockBytes * 8) / dataBits
  def fetchBytes = dataBits / 8
  def refillCycles = cacheDataBeats
  def vpnBits: Int = vaddrBits - pgIdxBits
}

class ICacheReq(vaddrBits: Int) extends Bundle {
  val addr = UInt(vaddrBits.W)
}

class ICacheResp(dataBits: Int) extends Bundle {
  val data = UInt(dataBits.W)
  val replay = Bool()
  val ae = Bool()
}

class ICacheBundle(cfg: ICacheParams) extends Bundle {
  val req = Flipped(Decoupled(new ICacheReq(cfg.vaddrBits)))
  val s1_paddr = Input(UInt(cfg.paddrBits.W))
  val s1_kill = Input(Bool())
  val s2_kill = Input(Bool())
  val s2_cacheable = Input(Bool())
  val s2_prefetch = Input(Bool())
  val resp = Valid(new ICacheResp(cfg.dataBits))
  val invalidate = Input(Bool())
}

class ICache(
  val cfg: ICacheParams
)(
  implicit p: Parameters)
    extends LazyModule {
  lazy val module = new ICacheModule(this)
  val masterNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            sourceId = IdRange(0, 2), // 0=refill, 1=hint
            name = s"Core ${cfg.coreId} ICache"
          )
        ),
        requestFields = Seq()
      )
    )
  )

  val size = cfg.nSets * cfg.nWays * cfg.blockBytes

}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer) {
  val cfg = outer.cfg
  val io = IO(new ICacheBundle(cfg))
  val (tl_out, edge_out) = outer.masterNode.out(0)

  val tECC = cfg.tagCode
  val dECC = cfg.dataCode
  require(isPow2(cfg.nSets) && isPow2(cfg.nWays))

  /** valid signal for CPU accessing cache in stage 0. */
  val s0_valid = io.req.fire

  /** virtual address from CPU in stage 0. */
  val s0_vaddr = io.req.bits.addr

  /** valid signal for stage 1, drived by s0_valid. */
  val s1_valid = RegInit(false.B)

  /** virtual address from CPU in stage 1. */
  val s1_vaddr = RegEnable(s0_vaddr, s0_valid)

  /** tag hit vector to indicate hit which way. */
  val s1_tag_hit = Wire(Vec(cfg.nWays, Bool()))

  val s1_hit = s1_tag_hit.reduce(_ || _)
  dontTouch(s1_hit)
  val s2_valid = RegNext(s1_valid && !io.s1_kill, false.B)
  val s2_hit = RegNext(s1_hit)

  /** status register to indicate a cache flush. */
  val invalidated = Reg(Bool())
  val refill_valid = RegInit(false.B)

  /** register to indicate [[tl_out]] is performing a hint. prefetch only happens after refilling
    */
  val send_hint = RegInit(false.B)

  /** indicate [[tl_out]] is performing a refill. */
  val refill_fire = tl_out.a.fire && !send_hint

  /** register to indicate there is a outstanding hint. */
  val hint_outstanding = RegInit(false.B)

  /** [[io]] access L1 I$ miss. */
  val s2_miss = s2_valid && !s2_hit && !io.s2_kill

  /** forward signal to stage 1, permit stage 1 refill. */
  val s1_can_request_refill = !(s2_miss || refill_valid)

  /** real refill signal, stage 2 miss, and was permit to refill in stage 1. Since a miss will trigger burst. miss under
    * miss won't trigger another burst.
    */
  val s2_request_refill = s2_miss && RegNext(s1_can_request_refill)
  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && s1_can_request_refill)
  val refill_vaddr = RegEnable(s1_vaddr, s1_valid && s1_can_request_refill)
  val refill_tag = refill_paddr >> cfg.pgUntagBits
  val refill_idx = index(refill_vaddr, refill_paddr)

  /** AccessAckData, is refilling I$, it will block request from CPU. */
  val refill_one_beat = tl_out.d.fire && edge_out.hasData(tl_out.d.bits)

  /** block request from CPU when refill or scratch pad access. */
  io.req.ready := !(refill_one_beat)
  s1_valid := s0_valid

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)

  /** at last beat of `tl_out.d.fire`, finish refill. */
  val refill_done = refill_one_beat && d_done

  /** scratchpad is writing data. block refill. */
  tl_out.d.ready := true.B

  require(edge_out.manager.minLatency > 0)

  /** way to be replaced, implemented with a hardcoded random replacement algorithm */
  val repl_way =
    if (cfg.isDM) 0.U
    else {
      // pick a way that is not used by the scratchpad
      val v0 = LFSR(16, refill_fire)(log2Up(cfg.nWays) - 1, 0)
      v0
    }

  /** Tag SRAM, indexed with virtual memory, content with `refillError ## tag[19:0]` after ECC
    */
  val tag_array = DescribedSRAM(
    name = "tag_array",
    desc = "ICache Tag Array",
    size = cfg.nSets,
    data = Vec(cfg.nWays, UInt(tECC.width(1 + cfg.tagBits).W))
  )
  val tag_rdata = tag_array.read(s0_vaddr(cfg.untagBits - 1, cfg.blockOffBits), !refill_done && s0_valid)

  /** register indicates the ongoing GetAckData transaction is corrupted. */
  val accruedRefillError = Reg(Bool())

  /** wire indicates the ongoing GetAckData transaction is corrupted. */
  val refillError = tl_out.d.bits.corrupt || (refill_cnt > 0.U && accruedRefillError)
  when(refill_done) {
    // For AccessAckData, denied => corrupt
    /** data written to [[tag_array]]. ECC encoded `refillError ## refill_tag`
      */
    val enc_tag = tECC.encode(Cat(refillError, refill_tag))
    tag_array.write(refill_idx, VecInit(Seq.fill(cfg.nWays) { enc_tag }), Seq.tabulate(cfg.nWays)(repl_way === _.U))

  }
  // notify CPU, I$ has corrupt.
  // io.errors.bus.valid := tl_out.d.fire && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
  // io.errors.bus.bits := (refill_paddr >> blockOffBits) << blockOffBits

  /** true indicate this cacheline is valid, indexed by (wayIndex ## setIndex) after refill_done and not FENCE.I,
    * (repl_way ## refill_idx) set to true.
    */
  val vb_array = RegInit(0.U((cfg.nSets * cfg.nWays).W))
  when(refill_one_beat) {
    accruedRefillError := refillError
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  /** flush cache when invalidate is true. */
  val invalidate = WireDefault(io.invalidate)
  when(invalidate) {
    vb_array := 0.U
    invalidated := true.B
  }

  /** wire indicates that tag is correctable or uncorrectable. will trigger CPU to replay and I$ invalidating, if
    * correctable.
    */
  val s1_tag_disparity = Wire(Vec(cfg.nWays, Bool()))

  /** wire indicates that bus has an uncorrectable error. respond to CPU [[io.resp.bits.ae]], cause
    * [[Causes.fetch_access]].
    */
  val s1_tl_error = Wire(Vec(cfg.nWays, Bool()))

  /** how many bits will be fetched by CPU for each fetch. */
  val wordBits = cfg.fetchBytes * 8

  /** a set of raw data read from [[data_arrays]]. */
  val s1_dout = Wire(Vec(cfg.nWays, UInt(dECC.width(wordBits).W)))
  s1_dout := DontCare

  // /** address accessed by [[tl_in]] for ITIM. */
  // val s0_slaveAddr = tl_in.map(_.a.bits.address).getOrElse(0.U)
  // /** address used at stage 1 and 3.
  //   * {{{
  //   * In stage 1, it caches TileLink data, store in stage 2 if ECC passed.
  //   * In stage 3, it caches corrected data from stage 2, and store in stage 4.}}}
  //   */
  // val s1s3_slaveAddr = Reg(UInt(log2Ceil(outer.size).W))
  // /** data used at stage 1 and 3.
  //   * {{{
  //   * In stage 1, it caches TileLink data, store in stage 2.
  //   * In stage 3, it caches corrected data from data ram, and return to d channel.}}}
  //   */
  // val s1s3_slaveData = Reg(UInt(wordBits.W))

  for (i <- 0 until cfg.nWays) {
    val s1_idx = index(s1_vaddr, io.s1_paddr)
    val s1_tag = io.s1_paddr >> cfg.pgUntagBits

    /** this way is used by scratchpad. [[tag_array]] corrupted.
      */
    // val scratchpadHit = scratchpadWayValid(i.U) &&
    //   Mux(s1_slaveValid,
    //     // scratchpad accessing form [[tl_in]].
    //     // @todo I think XBar will guarantee there won't be an illegal access on the bus?
    //     //       so why did have this check `lineInScratchpad(scratchpadLine(s1s3_slaveAddr))`?
    //     //       I think it will always be true.
    //     lineInScratchpad(scratchpadLine(s1s3_slaveAddr)) && scratchpadWay(s1s3_slaveAddr) === i.U,
    //     // scratchpad accessing from [[io]].
    //     // @todo Accessing ITIM correspond address will be able to read cacheline?
    //     //       is this desired behavior?
    //     addrInScratchpad(io.s1_paddr) && scratchpadWay(io.s1_paddr) === i.U)
    val s1_vb = vb_array(Cat(i.U, s1_idx))
    // printf(cf"width is variable because of i.U  ${Cat(i.U, s1_idx).getWidth}\n")
    val enc_tag = tECC.decode(tag_rdata(i))

    /** [[tl_error]] ECC error bit. [[tag]] of [[tag_array]] access.
      */
    val (tl_error, tag) = Split(enc_tag.uncorrected, cfg.tagBits)
    val tagMatch = s1_vb && tag === s1_tag

    /** tag error happens. */
    s1_tag_disparity(i) := s1_vb && enc_tag.error

    /** if tag matched but ecc checking failed, this access will trigger [[Causes.fetch_access]] exception. */
    s1_tl_error(i) := tagMatch && tl_error.asBool
    s1_tag_hit(i) := tagMatch
  }
  assert(!(s1_valid) || PopCount(s1_tag_hit.zip(s1_tag_disparity).map { case (h, d) => h && !d }) <= 1.U)

  println(s"tl width ${tl_out.d.bits.data.getWidth}")
  println(s"tl mask width ${tl_out.a.bits.mask.getWidth}")
  require(tl_out.d.bits.data.getWidth % wordBits == 0)

  /** Data SRAM
    *
    * banked with TileLink beat bytes / CPU fetch bytes, indexed with [[index]] and multi-beats cycle, content with
    * `eccError ## wordBits` after ECC.
    * {{{
    * │                          │xx│xxxxxx│xxx│x│xx│
    *                                            ↑word
    *                                          ↑bank
    *                            ↑way
    *                               └─set──┴─offset─┘
    *                               └────row───┘
    * }}}
    * Note: Data SRAM is indexed with virtual memory(vaddr[11:2]),
    *   - vaddr[11:3]->row,
    *   - vaddr[2]->bank=i
    *   - Cache line size = refillCycels(8) * bank(2) * datasize(4 bytes) = 64 bytes
    *   - data width = 32
    *
    * read: read happens in stage 0
    *
    * write: It takes 8 beats to refill 16 instruction in each refilling cycle. Data_array receives data[63:0](2
    * instructions) at once,they will be allocated in deferent bank according to vaddr[2]
    */
  val data_arrays = Seq.tabulate(tl_out.d.bits.data.getWidth / wordBits) { i =>
    DescribedSRAM(
      name = s"data_arrays_${i}",
      desc = "ICache Data Array",
      size = cfg.nSets * cfg.refillCycles,
      data = Vec(cfg.nWays, UInt(dECC.width(wordBits).W))
    )
  }

  for ((data_array, i) <- data_arrays.zipWithIndex) {

    /** bank match (vaddr[2]) */
    def wordMatch(addr: UInt) =
      addr.extract(log2Ceil(tl_out.d.bits.data.getWidth / 8) - 1, log2Ceil(wordBits / 8)) === i.U
    def row(addr: UInt) = addr(cfg.untagBits - 1, cfg.blockOffBits - log2Ceil(cfg.refillCycles))

    /** read_enable signal */
    val s0_ren = (s0_valid && wordMatch(s0_vaddr))

    /** write_enable signal refill from [[tl_out]] or ITIM write.
      */
    val wen = (refill_one_beat && !invalidated)

    /** index to access [[data_array]]. */
    val mem_idx =
      // I$ refill. refill_idx[2:0] is the beats
      Mux(
        refill_one_beat,
        (refill_idx << log2Ceil(cfg.refillCycles)) | refill_cnt,
        // CPU read.
        row(s0_vaddr)
      )
    when(wen) {
      // wr_data
      val data = tl_out.d.bits.data(wordBits * (i + 1) - 1, wordBits * i)
      // the way to be replaced/written
      val way = repl_way
      data_array.write(
        mem_idx,
        VecInit(Seq.fill(cfg.nWays) { dECC.encode(data) }),
        (0 until cfg.nWays).map(way === _.U)
      )
    }
    // write access
    /** data read from [[data_array]]. */
    val dout = data_array.read(mem_idx, !wen && s0_ren)
    // Mux to select a way to [[s1_dout]]
    when(wordMatch(io.s1_paddr)) {
      s1_dout := dout
    }
  }

  /** When writing full words to ITIM, ECC errors are correctable. When writing a full scratchpad word, suppress the
    * read so Xs don't leak out
    */
  // val s1s2_full_word_write = WireDefault(false.B)
  // val s1_dont_read = s1_slaveValid && s1s2_full_word_write

  /** clock gate signal for [[s2_tag_hit]], [[s2_dout]], [[s2_tag_disparity]], [[s2_tl_error]], [[s2_scratchpad_hit]].
    */
  val s1_clk_en = s1_valid
  val s2_tag_hit = RegEnable(s1_tag_hit, s1_clk_en)

  /** way index to access [[data_arrays]]. */
  val s2_hit_way = OHToUInt(s2_tag_hit)

  /** ITIM index to access [[data_arrays]]. replace tag with way, word set to 0.
    */
  val s2_dout = RegEnable(s1_dout, s1_clk_en)
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)
  val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_clk_en).asUInt.orR
  val s2_tl_error = RegEnable(s1_tl_error.asUInt.orR, s1_clk_en)

  /** ECC decode result for [[data_arrays]]. */
  val s2_data_decoded = dECC.decode(s2_way_mux)

  /** ECC error happened, correctable or uncorrectable, ask CPU to replay. */
  val s2_disparity = s2_tag_disparity || s2_data_decoded.error

  /** access hit in ITIM, if [[s1_slaveValid]], this access is from [[tl_in]], else from CPU [[io]]. */
  // val s1_scratchpad_hit = Mux(s1_slaveValid, lineInScratchpad(scratchpadLine(s1s3_slaveAddr)), addrInScratchpad(io.s1_paddr))
  /** stage 2 of [[s1_scratchpad_hit]]. */
  // val s2_scratchpad_hit = RegEnable(s1_scratchpad_hit, s1_clk_en)
  /** ITIM uncorrectable read. `s2_scratchpad_hit`: processing a scratchpad read(from [[tl_in]] or [[io]])
    * `s2_data_decoded.uncorrectable`: read a uncorrectable data. `s2_valid`: [[io]] non-canceled read. `(s2_slaveValid
    * && !s2_full_word_write)`: [[tl_in]] read or write a word with wormhole. if write a full word, even stage 2 read
    * uncorrectable. stage 3 full word write will recovery this.
    */
  // val s2_report_uncorrectable_error = s2_scratchpad_hit && s2_data_decoded.uncorrectable && (s2_valid || (s2_slaveValid && !s1s2_full_word_write))
  /** ECC uncorrectable address, send to Bus Error Unit. */
  // val s2_error_addr = scratchpadBase.map(base => Mux(s2_scratchpad_hit, base + s2_scratchpad_word_addr, 0.U)).getOrElse(0.U)

  // output signals
  outer.cfg.latency match {
    // if I$ latency is 1, no ITIM, no ECC.
    case 1 =>
      require(tECC.isInstanceOf[IdentityCode])
      require(dECC.isInstanceOf[IdentityCode])
      // reply data to CPU at stage 2. no replay.
      io.resp.bits.data := Mux1H(s1_tag_hit, s1_dout)
      io.resp.bits.ae := s1_tl_error.asUInt.orR
      io.resp.valid := s1_valid && s1_hit
      io.resp.bits.replay := false.B

    // if I$ latency is 2, can have ITIM and ECC.
    case 2 =>
      // when some sort of memory bit error have occurred
      // @todo why so aggressive to invalidate all when ecc corrupted.
      when(s2_valid && s2_disparity) { invalidate := true.B }

      // reply data to CPU at stage 2.
      io.resp.bits.data := s2_data_decoded.uncorrected
      io.resp.bits.ae := s2_tl_error
      io.resp.bits.replay := s2_disparity
      io.resp.valid := s2_valid && s2_hit

    // // report correctable error to BEU at stage 2.
    // io.errors.correctable.foreach { c =>
    //   c.valid := (s2_valid || s2_slaveValid) && s2_disparity
    //   c.bits := s2_error_addr
    // }
    // // report uncorrectable error to BEU at stage 2.
    // io.errors.uncorrectable.foreach { u =>
    //   u.valid := false.B
    //   u.bits := s2_error_addr
    // }

  }

  println(s"edge out bundle ${edge_out.bundle}")
  tl_out.a.valid := s2_request_refill
  tl_out.a.bits := edge_out
    .Get(
      fromSource = 0.U,
      toAddress = (refill_paddr >> cfg.blockOffBits) << cfg.blockOffBits,
      lgSize = cfg.lgCacheBlockBytes.U
    )
    ._2

  // // prefetch when next-line access does not cross a page
  // if (cacheParams.prefetch) {
  //   /** [[crosses_page]]  indicate if there is a crosses page access
  //     * [[next_block]] : the address to be prefetched.
  //     */
  //   val (crosses_page, next_block) = Split(refill_paddr(pgIdxBits-1, blockOffBits) +& 1.U, pgIdxBits-blockOffBits)

  //   when (tl_out.a.fire) {
  //     send_hint := !hint_outstanding && io.s2_prefetch && !crosses_page
  //     when (send_hint) {
  //       send_hint := false.B
  //       hint_outstanding := true.B
  //     }
  //   }

  //   // @todo why refill_done will kill hint at this cycle?
  //   when (refill_done) {
  //     send_hint := false.B
  //   }

  //   // D channel reply with HintAck.
  //   when (tl_out.d.fire && !refill_one_beat) {
  //     hint_outstanding := false.B
  //   }

  //   when (send_hint) {
  //     tl_out.a.valid := true.B
  //     tl_out.a.bits := edge_out.Hint(
  //                       fromSource = 1.U,
  //                       toAddress = Cat(refill_paddr >> pgIdxBits, next_block) << blockOffBits,
  //                       lgSize = lgCacheBlockBytes.U,
  //                       param = TLHints.PREFETCH_READ)._2
  //   }

  //   ccover(send_hint && !tl_out.a.ready, "PREFETCH_A_STALL", "I$ prefetch blocked by A-channel")
  //   ccover(refill_valid && (tl_out.d.fire && !refill_one_beat), "PREFETCH_D_BEFORE_MISS_D", "I$ prefetch resolves before miss")
  //   ccover(!refill_valid && (tl_out.d.fire && !refill_one_beat), "PREFETCH_D_AFTER_MISS_D", "I$ prefetch resolves after miss")
  //   ccover(tl_out.a.fire && hint_outstanding, "PREFETCH_D_AFTER_MISS_A", "I$ prefetch resolves after second miss")
  // }

  // Drive APROT information
  tl_out.a.bits.user.lift(AMBAProt).foreach { x =>
    // Rocket caches all fetch requests, and it's difficult to differentiate privileged/unprivileged on
    // cached data, so mark as privileged
    x.fetch := true.B
    x.secure := true.B
    x.privileged := true.B
    x.bufferable := true.B
    x.modifiable := true.B
    x.readalloc := io.s2_cacheable
    x.writealloc := io.s2_cacheable
  }
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B

  // if there is an outstanding refill, cannot flush I$.
  when(!refill_valid) { invalidated := false.B }
  when(refill_fire) { refill_valid := true.B }
  when(refill_done) { refill_valid := false.B }

  // io.perf.acquire := refill_fire
  // don't gate I$ clock since there are outstanding transcations.
  // io.keep_clock_enabled :=
  //   tl_in
  //     .map(tl => tl.a.valid || tl.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid)
  //     .getOrElse(false.B) || // ITIM
  //     s1_valid || s2_valid || refill_valid || send_hint || hint_outstanding // I$

  /** index to access [[data_arrays]] and [[tag_array]].
    * @note
    *   if [[untagBits]] > [[pgIdxBits]] in
    *   {{{
    *                        ┌──idxBits──┐
    *                        ↓           ↓
    * │          tag         │    set    │offset│
    * │              pageTag     │     pageIndex│
    *                        ↑   ↑       ↑      │
    *                   untagBits│  blockOffBits│
    *                       pgIdxBits    │
    *                        └msb┴──lsb──┘
    *                        vaddr paddr
    *   }}}
    *
    * else use paddr directly. Note: if [[untagBits]] > [[pgIdxBits]], there will be a alias issue which isn't
    * addressend by the icache yet.
    */
  def index(vaddr: UInt, paddr: UInt) = {

    /** [[paddr]] as LSB to be used for VIPT. */
    val lsbs = paddr(cfg.pgUntagBits - 1, cfg.blockOffBits)

    /** if [[untagBits]] > [[pgIdxBits]], append [[vaddr]] to higher bits of index as [[msbs]]. */
    val msbs = (cfg.idxBits + cfg.blockOffBits > cfg.pgUntagBits)
      .option(vaddr(cfg.idxBits + cfg.blockOffBits - 1, cfg.pgUntagBits))
    msbs ## lsbs
  }
}
