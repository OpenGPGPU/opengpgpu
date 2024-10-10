package ogpu.core

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.AMBAProt
import freechips.rocketchip.rocket.{
  isRead,
  isWrite,
  AMOALU,
  InlineInstance,
  LoadGen,
  M_FLUSH,
  M_FLUSH_ALL,
  M_HFENCEG,
  M_HFENCEV,
  M_PFW,
  M_PWR,
  M_SFENCE,
  M_SZ,
  M_WOK,
  M_XA_ADD,
  M_XA_AND,
  M_XA_MAX,
  M_XA_MAXU,
  M_XA_MIN,
  M_XA_MINU,
  M_XA_OR,
  M_XA_SWAP,
  M_XA_XOR,
  M_XLR,
  M_XSC,
  M_XWR,
  PRV,
  StoreGen
}
import freechips.rocketchip.util._

case class CacheParameter(
  nSets:                Int = 64,
  nWays:                Int = 4,
  paddrBits:            Int = 48,
  vaddrBits:            Int = 48,
  pgIdxBits:            Int = 12,
  pgLevelBits:          Int = 9,
  dataBits:             Int = 64,
  nTLBSets:             Int = 32,
  nTLBWays:             Int = 4,
  xLen:                 Int = 64,
  coreId:               Int = 0,
  pgLevels:             Int = 3,
  haveCFlush:           Boolean = false,
  tagECC:               Option[String] = None,
  dataECC:              Option[String] = None,
  dataECCBytes:         Int = 1,
  prefetch:             Boolean = false,
  usingAtomicsInCache:  Boolean = false,
  replacementPolicy:    String = "random",
  acquireBeforeRelease: Boolean = false,
  pipelineWayMux:       Boolean = false,
  blockBytes:           Int = 64,
  latency:              Int = 2) {
  def vpnBits:  Int = vaddrBits - pgIdxBits
  def tagCode:  Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = new RandomReplacement(nWays)
  def blockOffBits: Int = log2Ceil(blockBytes)
  def lgCacheBlockBytes = blockOffBits
  def cacheBlockBytes = blockBytes
  def untagBits:   Int = log2Ceil(nSets) + blockOffBits
  def tagBits:     Int = vaddrBits - untagBits
  def pgUntagBits: Int = untagBits
  def idxBits = log2Up(nSets)
  def isDM = nWays == 1
  def cacheDataBeats = (blockBytes * 8) / dataBits
  def fetchBytes = dataBits / 8
  def cacheDataBits = blockBytes * 8
  def refillCycles = cacheDataBeats
  def maxAddrBits = paddrBits.max(vaddrBits)
  def rowBits = xLen // rowBits is bus width
  def rowBytes = rowBits / 8
  def rowOffBits = log2Up(rowBytes)
  def wordBits = xLen
  def wordBytes = wordBits / 8
  def rowWords = rowBits / wordBits
  def eccBytes = dataECCBytes
  def eccBits = eccBytes * 8
  def encBits = dataCode.width(eccBits)
  def encWordBits = encBits * (wordBits / eccBits)
  def encDataBits = dataCode.width(xLen)
  def encRowBits = encDataBits * rowWords
  def subWordBits = wordBits // subbanking
  def subWordBytes = subWordBits / 8
  def dcacheReqTagBits: Int = 6
  def idxMSB = untagBits - 1
  def idxLSB = blockOffBits
  def lrscCycles = 80
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def beatBytes = fetchBytes
  def wordOffBits = log2Up(wordBytes)
  def beatWords = beatBytes / wordBytes
  def blockProbeAfterGrantCycles = 8
  def silentDrop: Boolean = !acquireBeforeRelease
  def usingAtomics = false
}

class CacheReq(cfg: CacheParameter) extends Bundle {
  val addr = UInt(cfg.maxAddrBits.W)
  // val idx = Some(UInt(cfg.maxAddrBits.W))
  val tag = UInt(cfg.dcacheReqTagBits.W)

  // val tag = UInt
  val cmd = UInt(M_SZ.W)
  val size = UInt(8.W)
  val signed = Bool()
  val dprv = UInt(PRV.SZ.W)
  val dv = Bool()
  val data = UInt(64.W)
  val mask = UInt(8.W)
  val phys = Bool()
  val no_alloc = Bool()
  val no_xcpt = Bool()
}

class CacheResp(cfg: CacheParameter) extends Bundle {
  val addr = UInt(cfg.maxAddrBits.W)
  // val tag = UInt
  val cmd = UInt(M_SZ.W)
  val size = UInt(8.W)
  val signed = Bool()
  val dprv = UInt(PRV.SZ.W)
  val dv = Bool()
  val data = UInt(64.W)
  val mask = UInt(8.W)
  val replay = Bool()
  val has_data = Bool()
  val data_word_bypass = UInt(8.W)
  val data_raw = UInt(8.W)
  val store_data = UInt(8.W)

}

class L1Metadata(cfg: CacheParameter) extends Bundle {
  val coh = new ClientMetadata
  val tag = UInt(cfg.tagBits.W)
}

object L1Metadata {
  def apply(
    cfg: CacheParameter,
    tag: Bits,
    coh: ClientMetadata
  )(
    implicit p: Parameters
  ) = {
    val meta = Wire(new L1Metadata(cfg))
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class CacheMetadataReq(cfg: CacheParameter) extends Bundle {
  val write = Bool()
  val addr = UInt(cfg.vaddrBits.W)
  val idx = UInt(cfg.idxBits.W)
  val way_en = UInt(cfg.nWays.W)
  val data = UInt(cfg.tagCode.width(new L1Metadata(cfg).getWidth).W)
}

class CacheWriteData(cfg: CacheParameter) extends Bundle {
  val data = UInt(64.W)
  val mask = UInt(8.W)
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class CacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
  val pf = new AlignmentExceptions
  val gf = new AlignmentExceptions
  val ae = new AlignmentExceptions
}

class CacheDataReq(cfg: CacheParameter) extends Bundle() {
  val addr = UInt(cfg.untagBits.W)
  val write = Bool()
  val wdata = UInt((cfg.encBits * cfg.rowBytes / cfg.eccBytes).W)
  val wordMask = UInt((cfg.rowBytes / cfg.subWordBytes).W)
  val eccMask = UInt((cfg.wordBytes / cfg.eccBytes).W)
  val way_en = UInt(cfg.nWays.W)
}

class CacheDataArray(cfg: CacheParameter) extends Module() {
  val io = IO(new Bundle {
    val req = Flipped(Valid(new CacheDataReq(cfg)))
    val resp = Output(Vec(cfg.nWays, UInt((req.bits.wdata.getWidth).W)))
  })

  require(cfg.rowBits % cfg.subWordBits == 0, "rowBits must be a multiple of subWordBits")
  val eccMask = if (cfg.eccBits == cfg.subWordBits) Seq(true.B) else io.req.bits.eccMask.asBools
  val wMask = if (cfg.nWays == 1) eccMask else (0 until cfg.nWays).flatMap(i => eccMask.map(_ && io.req.bits.way_en(i)))
  val wWords = io.req.bits.wdata.grouped(cfg.encBits * (cfg.subWordBits / cfg.eccBits))
  val addr = io.req.bits.addr >> cfg.rowOffBits
  val data_arrays = Seq.tabulate(cfg.rowBits / cfg.subWordBits) { i =>
    DescribedSRAM(
      name = s"data_arrays_${i}",
      desc = "DCache Data Array",
      size = cfg.nSets * cfg.cacheBlockBytes / cfg.rowBytes,
      data = Vec(cfg.nWays * (cfg.subWordBits / cfg.eccBits), UInt(cfg.encBits.W))
    )
  }

  val rdata = for ((array, i) <- data_arrays.zipWithIndex) yield {
    val valid = io.req.valid && ((data_arrays.size == 1).B || io.req.bits.wordMask(i))
    when(valid && io.req.bits.write) {
      val wMaskSlice = (0 until wMask.size)
        .filter(j =>
          i % (cfg.wordBits / cfg.subWordBits) == (j % (cfg.wordBytes / cfg.eccBytes)) / (cfg.subWordBytes / cfg.eccBytes)
        )
        .map(wMask(_))
      val wData = wWords(i).grouped(cfg.encBits)
      array.write(addr, VecInit((0 until cfg.nWays).flatMap(_ => wData)), wMaskSlice)
    }
    val data = array.read(addr, valid && !io.req.bits.write)
    data.grouped(cfg.subWordBits / cfg.eccBits).map(_.asUInt).toSeq
  }
  (io.resp.zip(rdata.transpose)).foreach { case (resp, data) => resp := data.asUInt }
}

class CacheIO(cfg: CacheParameter) extends Bundle {
  val req = Decoupled(new CacheReq(cfg))
  val s1_kill = Output(Bool())
  val s1_data = Output(new CacheWriteData(cfg))
  val s2_nack = Input(Bool())
  val s2_nack_cause_raw = Input(Bool())
  val s2_kill = Output(Bool())
  val s2_uncached = Input(Bool())
  val s2_paddr = Input(UInt(cfg.paddrBits.W))

  val resp = Flipped(Valid(new CacheResp(cfg)))
  val replay_next = Input(Bool())
  val s2_xcpt = Input(new CacheExceptions)
  // val uncached_resp = Some(Flipped(Decoupled(new CacheResp(cfg))))
  val ordered = Input(Bool())

  // val keep_clock_enabled = Output(Bool())
  // val clock_enabled = Input(Bool())
}

class CacheBundle(cfg: CacheParameter) extends Bundle {
  val cpu = Flipped(new CacheIO(cfg))
  val ptw = new TLBPTWIO(cfg.vpnBits, cfg.vaddrBits, cfg.pgLevels)
}

class DCache(
  val cfg: CacheParameter
)(
  implicit p: Parameters)
    extends LazyModule {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            sourceId = IdRange(0, 2), // 0=refill, 1=hint
            name = s"Core ${cfg.coreId} DCache",
            supportsProbe = TransferSizes(cfg.blockBytes, cfg.blockBytes)
          )
        ),
        requestFields = Seq()
      )
    )
  )

  def flushOnFenceI = !node.edges
    .out(0)
    .manager
    .managers
    .forall(m =>
      !m.supportsAcquireB || !m.executable || m.regionType >= RegionType.TRACKED || m.regionType <= RegionType.IDEMPOTENT
    )

  lazy val module = new DCacheModule(this)
}

class DCacheModule(outer: DCache) extends LazyModuleImp(outer) {
  val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)
  val cfg = outer.cfg
  val io = IO(new CacheBundle(cfg))

  val tECC = cfg.tagCode
  val dECC = cfg.dataCode
  val usingRMW = cfg.eccBytes > 1 || cfg.usingAtomicsInCache
  val tlb_param =
    TLBParameter(nSets = cfg.nTLBSets, nWays = cfg.nTLBWays, paddrBits = cfg.paddrBits, vaddrBits = cfg.vaddrBits)

  val tlb = Module(new TLB(false, tlb_param))

  // tags
  val replacer = ReplacementPolicy.fromString(cfg.replacementPolicy, cfg.nWays)
  val metaArb = Module(new Arbiter(new CacheMetadataReq(cfg), 8) with InlineInstance)

  val tag_array = DescribedSRAM(
    name = "tag_array",
    desc = "DCache Tag Array",
    size = cfg.nSets,
    data = Vec(cfg.nWays, chiselTypeOf(metaArb.io.out.bits.data))
  )

  // data
  val data = Module(new CacheDataArray(cfg))
  val dataArb = Module(new Arbiter(new CacheDataReq(cfg), 4) with InlineInstance)
  dataArb.io.in.tail.foreach(_.bits.wdata := dataArb.io.in.head.bits.wdata) // tie off write ports by default
  data.io.req.bits <> dataArb.io.out.bits
  data.io.req.valid := dataArb.io.out.valid
  dataArb.io.out.ready := true.B
  metaArb.io.out.ready := true.B

  val tl_out_a = Wire(chiselTypeOf(tl_out.a))
  tl_out.a <> {
    val a_queue_depth = 1
    Queue(tl_out_a, a_queue_depth, flow = true)
  }

  val (tl_out_c, release_queue_empty) =
    if (cfg.acquireBeforeRelease) {
      val q = Module(new Queue(chiselTypeOf(tl_out.c.bits), cfg.cacheDataBeats, flow = true))
      tl_out.c <> q.io.deq
      (q.io.enq, q.io.count === 0.U)
    } else {
      (tl_out.c, true.B)
    }

  val s1_valid = RegNext(io.cpu.req.fire, false.B)
  val s1_probe = RegNext(tl_out.b.fire, false.B)
  val probe_bits = RegEnable(tl_out.b.bits, tl_out.b.fire) // TODO has data now :(
  val s1_nack = WireDefault(false.B)
  val s1_valid_masked = s1_valid && !io.cpu.s1_kill
  val s1_valid_not_nacked = s1_valid && !s1_nack
  // val s1_tlb_req_valid = RegNext(tlb_port.req.fire, false.B)
  // val s2_tlb_req_valid = RegNext(s1_tlb_req_valid, false.B)
  val s0_clk_en = metaArb.io.out.valid && !metaArb.io.out.bits.write

  val s0_req = WireInit(io.cpu.req.bits)
  s0_req.addr := Cat(metaArb.io.out.bits.addr >> cfg.blockOffBits, io.cpu.req.bits.addr(cfg.blockOffBits - 1, 0))
  // s0_req.idx.foreach(_ := Cat(metaArb.io.out.bits.idx, s0_req.addr(cfg.blockOffBits - 1, 0)))
  when(!metaArb.io.in(7).ready) { s0_req.phys := true.B }
  val s1_req = RegEnable(s0_req, s0_clk_en)
  val s1_vaddr = Cat(s1_req.addr >> tagLSB, s1_req.addr(tagLSB - 1, 0))

  val s0_tlb_req = WireInit(tlb.io.req.bits)
  s0_tlb_req.passthrough := s0_req.phys
  s0_tlb_req.vaddr := s0_req.addr
  s0_tlb_req.size := s0_req.size
  s0_tlb_req.cmd := s0_req.cmd
  s0_tlb_req.prv := s0_req.dprv
  // s0_tlb_req.v := s0_req.dv
  val s1_tlb_req = RegEnable(s0_tlb_req, s0_clk_en)

  val s1_read = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write
  val s1_sfence = s1_req.cmd === M_SFENCE || s1_req.cmd === M_HFENCEV || s1_req.cmd === M_HFENCEG
  val s1_flush_line = s1_req.cmd === M_FLUSH_ALL && s1_req.size(0)
  val s1_flush_valid = Reg(Bool())
  val s1_waw_hazard = Wire(Bool())

  val s_ready :: s_voluntary_writeback :: s_probe_rep_dirty :: s_probe_rep_clean :: s_probe_retry :: s_probe_rep_miss :: s_voluntary_write_meta :: s_probe_write_meta :: s_dummy :: s_voluntary_release :: Nil =
    Enum(10)
  val supports_flush = outer.flushOnFenceI || cfg.haveCFlush
  val flushed = RegInit(true.B)
  val flushing = RegInit(false.B)
  val flushing_req = Reg(chiselTypeOf(s1_req))
  val cached_grant_wait = RegInit(false.B)
  val resetting = RegInit(false.B)
  val flushCounter = RegInit((cfg.nSets * (cfg.nWays - 1)).U(log2Ceil(cfg.nSets * cfg.nWays).W))
  val release_ack_wait = RegInit(false.B)
  val release_ack_addr = Reg(UInt(cfg.paddrBits.W))
  val release_state = RegInit(s_ready)
  val refill_way = Reg(UInt())
  val any_pstore_valid = Wire(Bool())
  val inWriteback = release_state.isOneOf(s_voluntary_writeback, s_probe_rep_dirty)
  val releaseWay = Wire(UInt())
  io.cpu.req.ready := (release_state === s_ready) && !cached_grant_wait && !s1_nack

  // hit initiation path
  val s0_read = isRead(io.cpu.req.bits.cmd)
  dataArb.io.in(3).valid := io.cpu.req.valid && likelyNeedsRead(io.cpu.req.bits)
  dataArb.io.in(3).bits := dataArb.io.in(1).bits
  dataArb.io.in(3).bits.write := false.B
  dataArb.io.in(3).bits.addr := Cat(
    io.cpu.req.bits.addr >> tagLSB,
    io.cpu.req.bits.addr(tagLSB - 1, 0)
  )
  dataArb.io.in(3).bits.wordMask := {
    val mask = (cfg.subWordBytes.log2 until cfg.rowOffBits).foldLeft(1.U) { case (in, i) =>
      val upper_mask = Mux(
        (i >= cfg.wordBytes.log2).B || io.cpu.req.bits.size <= i.U,
        0.U,
        ((BigInt(1) << (1 << (i - cfg.subWordBytes.log2))) - 1).U
      )
      val upper = Mux(io.cpu.req.bits.addr(i), in, 0.U) | upper_mask
      val lower = Mux(io.cpu.req.bits.addr(i), 0.U, in)
      upper ## lower
    }
    Fill(cfg.subWordBytes / cfg.eccBytes, mask)
  }
  dataArb.io.in(3).bits.eccMask := ~0.U((cfg.wordBytes / cfg.eccBytes).W)
  dataArb.io.in(3).bits.way_en := ~0.U(cfg.nWays.W)
  when(!dataArb.io.in(3).ready && s0_read) { io.cpu.req.ready := false.B }
  val s1_did_read = RegEnable(dataArb.io.in(3).ready && (io.cpu.req.valid && needsRead(io.cpu.req.bits)), s0_clk_en)
  val s1_read_mask = RegEnable(dataArb.io.in(3).bits.wordMask, s0_clk_en)

  metaArb.io.in(7).valid := io.cpu.req.valid
  metaArb.io.in(7).bits.write := false.B
  metaArb.io.in(7).bits.idx := dataArb.io.in(3).bits.addr(cfg.idxMSB, cfg.idxLSB)
  metaArb.io.in(7).bits.addr := io.cpu.req.bits.addr
  metaArb.io.in(7).bits.way_en := metaArb.io.in(4).bits.way_en
  metaArb.io.in(7).bits.data := metaArb.io.in(4).bits.data
  when(!metaArb.io.in(7).ready) { io.cpu.req.ready := false.B }

  // address translation
  val s1_cmd_uses_tlb = s1_readwrite || s1_flush_line || s1_req.cmd === M_WOK
  io.ptw <> tlb.io.ptw
  tlb.io.kill := io.cpu.s2_kill
  tlb.io.req.valid := s1_valid && !io.cpu.s1_kill && s1_cmd_uses_tlb
  tlb.io.req.bits := s1_tlb_req
  when(!tlb.io.req.ready && !tlb.io.ptw.resp.valid && !io.cpu.req.bits.phys) { io.cpu.req.ready := false.B }
  when(s1_valid && s1_cmd_uses_tlb && tlb.io.resp.miss) { s1_nack := true.B }

  tlb.io.sfence.valid := s1_valid && !io.cpu.s1_kill && s1_sfence
  tlb.io.sfence.bits.rs1 := s1_req.size(0)
  tlb.io.sfence.bits.rs2 := s1_req.size(1)
  tlb.io.sfence.bits.asid := io.cpu.s1_data.data
  tlb.io.sfence.bits.addr := s1_req.addr
  // tlb.io.sfence.bits.hv := s1_req.cmd === M_HFENCEV
  // tlb.io.sfence.bits.hg := s1_req.cmd === M_HFENCEG

  val s1_paddr = Cat(tlb.io.resp.paddr(cfg.paddrBits - 1, cfg.pgIdxBits), s1_req.addr(cfg.pgIdxBits - 1, 0))
  val s1_victim_way = Wire(UInt())

  val (s1_hit_way, s1_hit_state, s1_meta) = {
    val metaReq = metaArb.io.out
    val metaIdx = metaReq.bits.idx
    when(metaReq.valid && metaReq.bits.write) {
      val wmask = if (cfg.nWays == 1) Seq(true.B) else metaReq.bits.way_en.asBools
      tag_array.write(metaIdx, VecInit(Seq.fill(cfg.nWays)(metaReq.bits.data)), wmask)
    }
    val s1_meta = tag_array.read(metaIdx, metaReq.valid && !metaReq.bits.write)
    val s1_meta_uncorrected = s1_meta.map(tECC.decode(_).uncorrected.asTypeOf(new L1Metadata(cfg)))
    val s1_tag = s1_paddr >> tagLSB
    val s1_meta_hit_way = s1_meta_uncorrected.map(r => r.coh.isValid() && r.tag === s1_tag).asUInt
    val s1_meta_hit_state = (s1_meta_uncorrected
      .map(r => Mux(r.tag === s1_tag && !s1_flush_valid, r.coh.asUInt, 0.U))
      .reduce(_ | _))
      .asTypeOf(chiselTypeOf(ClientMetadata.onReset))
    (s1_meta_hit_way, s1_meta_hit_state, s1_meta)
  }

  val s1_data_way = WireDefault(if (cfg.nWays == 1) 1.U else Mux(inWriteback, releaseWay, s1_hit_way))
  val tl_d_data_encoded = Wire(chiselTypeOf(encodeData(tl_out.d.bits.data, false.B)))
  val s1_all_data_ways = VecInit(data.io.resp ++ None)
  val s1_mask_xwr = new StoreGen(s1_req.size, s1_req.addr, 0.U, cfg.wordBytes).mask
  val s1_mask = Mux(s1_req.cmd === M_PWR, io.cpu.s1_data.mask, s1_mask_xwr)
  // for partial writes, s1_data.mask must be a subset of s1_mask_xwr
  assert(!(s1_valid_masked && s1_req.cmd === M_PWR) || (s1_mask_xwr | ~io.cpu.s1_data.mask).andR)

  val s2_valid = RegNext(s1_valid_masked && !s1_sfence, init = false.B)
  val s2_valid_no_xcpt = s2_valid && !io.cpu.s2_xcpt.asUInt.orR
  val s2_probe = RegNext(s1_probe, init = false.B)
  val releaseInFlight = s1_probe || s2_probe || release_state =/= s_ready
  val s2_not_nacked_in_s1 = RegNext(!s1_nack)
  val s2_valid_not_nacked_in_s1 = s2_valid && s2_not_nacked_in_s1
  val s2_valid_masked = s2_valid_no_xcpt && s2_not_nacked_in_s1
  val s2_valid_not_killed = s2_valid_masked && !io.cpu.s2_kill
  val s2_req = Reg(chiselTypeOf(io.cpu.req.bits))
  val s2_cmd_flush_all = s2_req.cmd === M_FLUSH_ALL && !s2_req.size(0)
  val s2_cmd_flush_line = s2_req.cmd === M_FLUSH_ALL && s2_req.size(0)
  val s2_tlb_xcpt = Reg(chiselTypeOf(tlb.io.resp))
  val s2_pma = Reg(chiselTypeOf(tlb.io.resp))
  val s2_uncached_resp_addr = Reg(chiselTypeOf(s2_req.addr)) // should be DCE'd in synthesis
  when(s1_valid_not_nacked || s1_flush_valid) {
    s2_req := s1_req
    s2_req.addr := s1_paddr
    s2_tlb_xcpt := tlb.io.resp
    s2_pma := tlb.io.resp
  }
  val s2_vaddr = Cat(RegEnable(s1_vaddr, s1_valid_not_nacked || s1_flush_valid) >> tagLSB, s2_req.addr(tagLSB - 1, 0))
  val s2_read = isRead(s2_req.cmd)
  val s2_write = isWrite(s2_req.cmd)
  val s2_readwrite = s2_read || s2_write
  val s2_flush_valid_pre_tag_ecc = RegNext(s1_flush_valid)
  val s1_meta_decoded = s1_meta.map(tECC.decode(_))
  val s1_meta_clk_en = s1_valid_not_nacked || s1_flush_valid || s1_probe
  val s2_meta_correctable_errors = s1_meta_decoded.map(m => RegEnable(m.correctable, s1_meta_clk_en)).asUInt
  val s2_meta_uncorrectable_errors = s1_meta_decoded.map(m => RegEnable(m.uncorrectable, s1_meta_clk_en)).asUInt
  val s2_meta_error_uncorrectable = s2_meta_uncorrectable_errors.orR
  val s2_meta_corrected = s1_meta_decoded.map(m => RegEnable(m.corrected, s1_meta_clk_en).asTypeOf(new L1Metadata(cfg)))
  val s2_meta_error = (s2_meta_uncorrectable_errors | s2_meta_correctable_errors).orR
  val s2_flush_valid = s2_flush_valid_pre_tag_ecc && !s2_meta_error
  val s2_data = {
    val wordsPerRow = cfg.rowBits / cfg.subWordBits
    val en = s1_valid || inWriteback || io.cpu.replay_next
    val word_en = Mux(inWriteback, Fill(wordsPerRow, 1.U), Mux(s1_did_read, s1_read_mask, 0.U))
    val s1_way_words = s1_all_data_ways.map(_.grouped(dECC.width(cfg.eccBits) * (cfg.subWordBits / cfg.eccBits)))
    if (cfg.pipelineWayMux) {
      val s1_word_en = Mux(io.cpu.replay_next, 0.U, word_en)
      (for (i <- 0 until wordsPerRow) yield {
        val s2_way_en = RegEnable(Mux(s1_word_en(i), s1_data_way, 0.U), en)
        val s2_way_words = (0 until cfg.nWays).map(j => RegEnable(s1_way_words(j)(i), en && word_en(i)))
        (0 until cfg.nWays).map(j => Mux(s2_way_en(j), s2_way_words(j), 0.U)).reduce(_ | _)
      }).asUInt
    } else {
      val s1_word_en = word_en
      (for (i <- 0 until wordsPerRow) yield {
        RegEnable(Mux1H(Mux(s1_word_en(i), s1_data_way, 0.U), s1_way_words.map(_(i))), en)
      }).asUInt
    }
  }
  val s2_probe_way = RegEnable(s1_hit_way, s1_probe)
  val s2_probe_state = RegEnable(s1_hit_state, s1_probe)
  val s2_hit_way = RegEnable(s1_hit_way, s1_valid_not_nacked)
  val s2_hit_state = RegEnable(s1_hit_state, s1_valid_not_nacked || s1_flush_valid)
  val s2_waw_hazard = RegEnable(s1_waw_hazard, s1_valid_not_nacked)
  val s2_store_merge = Wire(Bool())
  val s2_hit_valid = s2_hit_state.isValid()
  val (s2_hit, s2_grow_param, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.cmd)
  val s2_data_decoded = decodeData(s2_data)
  val s2_word_idx = s2_req.addr.extract(log2Up(cfg.rowBits / 8) - 1, log2Up(cfg.wordBytes))
  val s2_data_error = s2_data_decoded.map(_.error).orR
  val s2_data_error_uncorrectable = s2_data_decoded.map(_.uncorrectable).orR
  val s2_data_corrected = (s2_data_decoded.map(_.corrected):     Seq[UInt]).asUInt
  val s2_data_uncorrected = (s2_data_decoded.map(_.uncorrected): Seq[UInt]).asUInt
  val s2_valid_hit_maybe_flush_pre_data_ecc_and_waw = s2_valid_masked && !s2_meta_error && s2_hit
  val s2_no_alloc_hazard = false.B
  val s2_valid_hit_pre_data_ecc_and_waw =
    s2_valid_hit_maybe_flush_pre_data_ecc_and_waw && s2_readwrite && !s2_no_alloc_hazard
  val s2_valid_flush_line = s2_valid_hit_maybe_flush_pre_data_ecc_and_waw && s2_cmd_flush_line
  val s2_valid_hit_pre_data_ecc = s2_valid_hit_pre_data_ecc_and_waw && (!s2_waw_hazard || s2_store_merge)
  val s2_valid_data_error = s2_valid_hit_pre_data_ecc_and_waw && s2_data_error
  val s2_valid_hit = s2_valid_hit_pre_data_ecc && !s2_data_error
  val s2_valid_miss = s2_valid_masked && s2_readwrite && !s2_meta_error && !s2_hit
  val s2_valid_cached_miss = s2_valid_miss
  dontTouch(s2_valid_cached_miss)
  val s2_want_victimize = (s2_valid_cached_miss || s2_valid_flush_line || s2_valid_data_error || s2_flush_valid)
  val s2_cannot_victimize = !s2_flush_valid && io.cpu.s2_kill
  val s2_victimize = s2_want_victimize && !s2_cannot_victimize
  val s2_victim_way = UIntToOH(RegEnable(s1_victim_way, s1_valid_not_nacked || s1_flush_valid))
  val s2_victim_or_hit_way = Mux(s2_hit_valid, s2_hit_way, s2_victim_way)
  val s2_victim_tag = Mux(
    s2_valid_data_error || s2_valid_flush_line,
    s2_req.addr(cfg.paddrBits - 1, tagLSB),
    Mux1H(s2_victim_way, s2_meta_corrected).tag
  )
  val s2_victim_state = Mux(s2_hit_valid, s2_hit_state, Mux1H(s2_victim_way, s2_meta_corrected).coh)

  val (s2_prb_ack_data, s2_report_param, probeNewCoh) = s2_probe_state.onProbe(probe_bits.param)
  val (s2_victim_dirty, s2_shrink_param, voluntaryNewCoh) = s2_victim_state.onCacheControl(M_FLUSH)
  dontTouch(s2_victim_dirty)
  val s2_update_meta = s2_hit_state =/= s2_new_hit_state
  // val s2_dont_nack_uncached = s2_valid_uncached_pending && tl_out_a.ready
  val s2_dont_nack_misc = s2_valid_masked && !s2_meta_error &&
    (supports_flush.B && s2_cmd_flush_all && flushed && !flushing ||
      supports_flush.B && s2_cmd_flush_line && !s2_hit ||
      s2_req.cmd === M_WOK)
  io.cpu.s2_nack := s2_valid_no_xcpt && !s2_dont_nack_misc && !s2_valid_hit
  when(io.cpu.s2_nack || (s2_valid_hit_pre_data_ecc_and_waw && s2_update_meta)) { s1_nack := true.B }

  // tag updates on ECC errors
  val s2_first_meta_corrected = PriorityMux(s2_meta_correctable_errors, s2_meta_corrected)
  metaArb.io.in(1).valid := s2_meta_error && (s2_valid_masked || s2_flush_valid_pre_tag_ecc || s2_probe)
  metaArb.io.in(1).bits.write := true.B
  metaArb.io.in(1).bits.way_en := s2_meta_uncorrectable_errors | Mux(
    s2_meta_error_uncorrectable,
    0.U,
    PriorityEncoderOH(s2_meta_correctable_errors)
  )
  metaArb.io.in(1).bits.idx := Mux(s2_probe, probeIdx(probe_bits), s2_vaddr(cfg.idxMSB, cfg.idxLSB))
  metaArb.io.in(1).bits.addr := Cat(
    io.cpu.req.bits.addr >> cfg.untagBits,
    metaArb.io.in(1).bits.idx << cfg.blockOffBits
  )
  metaArb.io.in(1).bits.data := tECC.encode {
    val new_meta = WireDefault(s2_first_meta_corrected)
    when(s2_meta_error_uncorrectable) { new_meta.coh := ClientMetadata.onReset }
    new_meta.asUInt
  }

  // tag updates on hit
  metaArb.io.in(2).valid := s2_valid_hit_pre_data_ecc_and_waw && s2_update_meta
  metaArb.io.in(2).bits.write := !io.cpu.s2_kill
  metaArb.io.in(2).bits.way_en := s2_victim_or_hit_way
  metaArb.io.in(2).bits.idx := s2_vaddr(cfg.idxMSB, cfg.idxLSB)
  metaArb.io.in(2).bits.addr := Cat(io.cpu.req.bits.addr >> cfg.untagBits, s2_vaddr(cfg.idxMSB, 0))
  metaArb.io.in(2).bits.data := tECC.encode(L1Metadata(cfg, s2_req.addr >> tagLSB, s2_new_hit_state).asUInt)

  // load reservations and TL error reporting
  val s2_lr = s2_req.cmd === M_XLR
  val s2_sc = s2_req.cmd === M_XSC
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount > cfg.lrscBackoff.U
  val lrscBackingOff = lrscCount > 0.U && !lrscValid
  val lrscAddr = Reg(UInt())
  val lrscAddrMatch = lrscAddr === (s2_req.addr >> cfg.blockOffBits)
  val s2_sc_fail = s2_sc && !(lrscValid && lrscAddrMatch)
  when((s2_valid_hit && s2_lr && !cached_grant_wait || s2_valid_cached_miss) && !io.cpu.s2_kill) {
    lrscCount := Mux(s2_hit, (cfg.lrscCycles - 1).U, 0.U)
    lrscAddr := s2_req.addr >> cfg.blockOffBits
  }
  when(lrscCount > 0.U) { lrscCount := lrscCount - 1.U }
  when(s2_valid_not_killed && lrscValid) { lrscCount := cfg.lrscBackoff.U }
  when(s1_probe) { lrscCount := 0.U }

  def s2_store_valid_pre_kill = s2_valid_hit && s2_write && !s2_sc_fail
  def s2_store_valid = s2_store_valid_pre_kill && !io.cpu.s2_kill
  val pstore1_cmd = RegEnable(s1_req.cmd, s1_valid_not_nacked && s1_write)
  val pstore1_addr = RegEnable(s1_vaddr, s1_valid_not_nacked && s1_write)
  val pstore1_data = RegEnable(io.cpu.s1_data.data, s1_valid_not_nacked && s1_write)
  val pstore1_way = RegEnable(s1_hit_way, s1_valid_not_nacked && s1_write)
  val pstore1_mask = RegEnable(s1_mask, s1_valid_not_nacked && s1_write)
  val pstore1_storegen_data = WireDefault(pstore1_data)
  val pstore1_rmw = usingRMW.B && RegEnable(needsRead(s1_req), s1_valid_not_nacked && s1_write)
  val pstore1_merge_likely = s2_valid_not_nacked_in_s1 && s2_write && s2_store_merge
  val pstore1_merge = s2_store_valid && s2_store_merge
  val pstore2_valid = RegInit(false.B)
  val pstore_drain_opportunistic =
    !(io.cpu.req.valid && likelyNeedsRead(io.cpu.req.bits)) && !(s1_valid && s1_waw_hazard)
  val pstore_drain_on_miss = releaseInFlight || RegNext(io.cpu.s2_nack)
  val pstore1_held = RegInit(false.B)
  val pstore1_valid_likely = s2_valid && s2_write || pstore1_held
  def pstore1_valid_not_rmw(s2_kill: Bool) = s2_valid_hit_pre_data_ecc && s2_write && !s2_kill || pstore1_held
  val pstore1_valid = s2_store_valid || pstore1_held
  any_pstore_valid := pstore1_held || pstore2_valid
  val pstore_drain_structural = pstore1_valid_likely && pstore2_valid && ((s1_valid && s1_write) || pstore1_rmw)
  assert(pstore1_rmw || pstore1_valid_not_rmw(io.cpu.s2_kill) === pstore1_valid)
  def should_pstore_drain(truly: Bool) = {
    val s2_kill = truly && io.cpu.s2_kill
    !pstore1_merge_likely &&
    (usingRMW.B && pstore_drain_structural ||
      (((pstore1_valid_not_rmw(
        s2_kill
      ) && !pstore1_rmw) || pstore2_valid) && (pstore_drain_opportunistic || pstore_drain_on_miss)))
  }
  val pstore_drain = should_pstore_drain(true.B)
  pstore1_held := (s2_store_valid && !s2_store_merge || pstore1_held) && pstore2_valid && !pstore_drain
  val advance_pstore1 = pstore1_valid && (pstore2_valid === pstore_drain)
  pstore2_valid := pstore2_valid && !pstore_drain || advance_pstore1
  val pstore2_addr = RegEnable(pstore1_addr, advance_pstore1)
  val pstore2_way = RegEnable(pstore1_way, advance_pstore1)
  val pstore2_storegen_data = {
    for (i <- 0 until cfg.wordBytes)
      yield RegEnable(
        pstore1_storegen_data(8 * (i + 1) - 1, 8 * i),
        advance_pstore1 || pstore1_merge && pstore1_mask(i)
      )
  }.asUInt
  val pstore2_storegen_mask = {
    val mask = Reg(UInt(cfg.wordBytes.W))
    when(advance_pstore1 || pstore1_merge) {
      val mergedMask = pstore1_mask | Mux(pstore1_merge, mask, 0.U)
      mask := mergedMask
    }
    mask
  }
  s2_store_merge := (if (cfg.eccBytes == 1) false.B
                     else {
                       // only merge stores to ECC granules that are already stored-to, to avoid
                       // WAW hazards
                       val wordMatch = (eccMask(pstore2_storegen_mask) | ~eccMask(pstore1_mask)).andR
                       val idxMatch = s2_vaddr(cfg.untagBits - 1, log2Ceil(cfg.wordBytes)) === pstore2_addr(
                         cfg.untagBits - 1,
                         log2Ceil(cfg.wordBytes)
                       )
                       val tagMatch = (s2_hit_way & pstore2_way).orR
                       pstore2_valid && wordMatch && idxMatch && tagMatch
                     })
  dataArb.io.in(0).valid := should_pstore_drain(false.B)
  dataArb.io.in(0).bits.write := pstore_drain
  dataArb.io.in(0).bits.addr := Mux(pstore2_valid, pstore2_addr, pstore1_addr)
  dataArb.io.in(0).bits.way_en := Mux(pstore2_valid, pstore2_way, pstore1_way)
  dataArb.io.in(0).bits.wdata := encodeData(
    Fill(cfg.rowWords, Mux(pstore2_valid, pstore2_storegen_data, pstore1_data)),
    false.B
  )
  dataArb.io.in(0).bits.wordMask := {
    val eccMask = dataArb.io.in(0).bits.eccMask.asBools.grouped(cfg.subWordBytes / cfg.eccBytes).map(_.orR).toSeq.asUInt
    val wordMask =
      UIntToOH(Mux(pstore2_valid, pstore2_addr, pstore1_addr).extract(cfg.rowOffBits - 1, cfg.wordBytes.log2))
    FillInterleaved(cfg.wordBytes / cfg.subWordBytes, wordMask) & Fill(cfg.rowBytes / cfg.wordBytes, eccMask)
  }
  dataArb.io.in(0).bits.eccMask := eccMask(Mux(pstore2_valid, pstore2_storegen_mask, pstore1_mask))

  // store->load RAW hazard detection
  def s1Depends(addr: UInt, mask: UInt) =
    addr(cfg.idxMSB, cfg.wordOffBits) === s1_vaddr(cfg.idxMSB, cfg.wordOffBits) &&
      Mux(s1_write, (eccByteMask(mask) & eccByteMask(s1_mask_xwr)).orR, (mask & s1_mask_xwr).orR)
  val s1_hazard =
    (pstore1_valid_likely && s1Depends(pstore1_addr, pstore1_mask)) ||
      (pstore2_valid && s1Depends(pstore2_addr, pstore2_storegen_mask))
  val s1_raw_hazard = s1_read && s1_hazard
  s1_waw_hazard := (if (cfg.eccBytes == 1) false.B
                    else {
                      s1_write && (s1_hazard || needsRead(s1_req) && !s1_did_read)
                    })
  when(s1_valid && s1_raw_hazard) { s1_nack := true.B }

  // performance hints to processor
  io.cpu.s2_nack_cause_raw := RegNext(s1_raw_hazard) || !(!s2_waw_hazard || s2_store_merge)

  // Prepare a TileLink request message that initiates a transaction
  val a_source = 0.U
  val acquire_address = (s2_req.addr >> cfg.idxLSB) << cfg.idxLSB
  val access_address = s2_req.addr
  val a_size = s2_req.size
  val a_data = Fill(cfg.beatWords, pstore1_data)
  val a_mask = pstore1_mask << (access_address.extract(cfg.beatBytes.log2 - 1, cfg.wordBytes.log2) << 3)
  val get = edge.Get(a_source, access_address, a_size)._2
  val put = edge.Put(a_source, access_address, a_size, a_data)._2
  val putpartial = edge.Put(a_source, access_address, a_size, a_data, a_mask)._2
  val atomics = if (edge.manager.anySupportLogical) {
    MuxLookup(s2_req.cmd, WireDefault(0.U.asTypeOf(new TLBundleA(edge.bundle))))(
      Array(
        M_XA_SWAP -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.SWAP)._2,
        M_XA_XOR -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.XOR)._2,
        M_XA_OR -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.OR)._2,
        M_XA_AND -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.AND)._2,
        M_XA_ADD -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.ADD)._2,
        M_XA_MIN -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MIN)._2,
        M_XA_MAX -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MAX)._2,
        M_XA_MINU -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MINU)._2,
        M_XA_MAXU -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MAXU)._2
      )
    )
  } else {
    WireDefault(new TLBundleA(edge.bundle), DontCare)
  }

  tl_out_a.valid := !io.cpu.s2_kill &&
    (s2_valid_cached_miss &&
      !(release_ack_wait && (s2_req.addr ^ release_ack_addr)(
        ((cfg.pgIdxBits + cfg.pgLevelBits).min(cfg.paddrBits)) - 1,
        cfg.idxLSB
      ) === 0.U) &&
      (cfg.acquireBeforeRelease.B && !release_ack_wait && release_queue_empty || !s2_victim_dirty))
  tl_out_a.bits := acquire(s2_vaddr, s2_req.addr, s2_grow_param)

  // Drive APROT Bits
  tl_out_a.bits.user.lift(AMBAProt).foreach { x =>
    val user_bit_cacheable = true.B

    x.privileged := s2_req.dprv === PRV.M.U || user_bit_cacheable
    // if the address is cacheable, enable outer caches
    x.bufferable := user_bit_cacheable
    x.modifiable := user_bit_cacheable
    x.readalloc := user_bit_cacheable
    x.writealloc := user_bit_cacheable

    // Following are always tied off
    x.fetch := false.B
    x.secure := true.B
  }

  when(tl_out_a.fire) {
    cached_grant_wait := true.B
    refill_way := s2_victim_or_hit_way
  }

  // grant
  val (d_first, d_last, d_done, d_address_inc) = edge.addr_inc(tl_out.d)
  val (d_opc, grantIsUncached, grantIsUncachedData) = {
    val uncachedGrantOpcodesSansData = Seq(AccessAck, HintAck)
    val uncachedGrantOpcodesWithData = Seq(AccessAckData)
    val uncachedGrantOpcodes = uncachedGrantOpcodesWithData ++ uncachedGrantOpcodesSansData
    val whole_opc = tl_out.d.bits.opcode
    (whole_opc, whole_opc.isOneOf(uncachedGrantOpcodes), whole_opc.isOneOf(uncachedGrantOpcodesWithData))
  }
  tl_d_data_encoded := encodeData(
    tl_out.d.bits.data,
    tl_out.d.bits.corrupt
  )
  val grantIsCached = d_opc.isOneOf(Grant, GrantData)
  val grantIsVoluntary = d_opc === ReleaseAck // Clears a different pending bit
  val grantIsRefill = d_opc === GrantData // Writes the data array
  val grantInProgress = RegInit(false.B)
  val blockProbeAfterGrantCount = RegInit(0.U)
  when(blockProbeAfterGrantCount > 0.U) { blockProbeAfterGrantCount := blockProbeAfterGrantCount - 1.U }
  val canAcceptCachedGrant = !release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta, s_voluntary_release)
  tl_out.d.ready := Mux(grantIsCached, (!d_first || tl_out.e.ready) && canAcceptCachedGrant, true.B)
  // uncachedResp := Mux1H(uncachedRespIdxOH, uncachedReqs)
  when(tl_out.d.fire) {
    when(grantIsCached) {
      grantInProgress := true.B
      assert(cached_grant_wait, "A GrantData was unexpected by the dcache.")
      when(d_last) {
        cached_grant_wait := false.B
        grantInProgress := false.B
        blockProbeAfterGrantCount := (cfg.blockProbeAfterGrantCycles - 1).U
        replacer.miss
      }
    }.elsewhen(grantIsVoluntary) {
      assert(
        release_ack_wait,
        "A ReleaseAck was unexpected by the dcache."
      ) // TODO should handle Ack coming back on same cycle!
      release_ack_wait := false.B
    }
  }

  // Finish TileLink transaction by issuing a GrantAck
  tl_out.e.valid := tl_out.d.valid && d_first && grantIsCached && canAcceptCachedGrant
  tl_out.e.bits := edge.GrantAck(tl_out.d.bits)
  assert(tl_out.e.fire === (tl_out.d.fire && d_first && grantIsCached))

  // data refill
  // note this ready-valid signaling ignores E-channel backpressure, which
  // benignly means the data RAM might occasionally be redundantly written
  dataArb.io.in(1).valid := tl_out.d.valid && grantIsRefill && canAcceptCachedGrant
  when(grantIsRefill && !dataArb.io.in(1).ready) {
    tl_out.e.valid := false.B
    tl_out.d.ready := false.B
  }
  dataArb.io.in(1).bits.write := true.B
  dataArb.io.in(1).bits.addr := (s2_vaddr >> cfg.idxLSB) << cfg.idxLSB | d_address_inc
  dataArb.io.in(1).bits.way_en := refill_way
  dataArb.io.in(1).bits.wdata := tl_d_data_encoded
  dataArb.io.in(1).bits.wordMask := ~0.U((cfg.rowBytes / cfg.subWordBytes).W)
  dataArb.io.in(1).bits.eccMask := ~0.U((cfg.wordBytes / cfg.eccBytes).W)

  // tag updates on refill
  // ignore backpressure from metaArb, which can only be caused by tag ECC
  // errors on hit-under-miss.  failing to write the new tag will leave the
  // line invalid, so we'll simply request the line again later.
  metaArb.io.in(3).valid := grantIsCached && d_done && !tl_out.d.bits.denied
  metaArb.io.in(3).bits.write := true.B
  metaArb.io.in(3).bits.way_en := refill_way
  metaArb.io.in(3).bits.idx := s2_vaddr(cfg.idxMSB, cfg.idxLSB)
  metaArb.io.in(3).bits.addr := Cat(io.cpu.req.bits.addr >> cfg.untagBits, s2_vaddr(cfg.idxMSB, 0))
  metaArb.io.in(3).bits.data := tECC.encode(
    L1Metadata(cfg, s2_req.addr >> tagLSB, s2_hit_state.onGrant(s2_req.cmd, tl_out.d.bits.param)).asUInt
  )

  // Handle an incoming TileLink Probe message
  val block_probe_for_core_progress = blockProbeAfterGrantCount > 0.U || lrscValid
  val block_probe_for_pending_release_ack = release_ack_wait && (tl_out.b.bits.address ^ release_ack_addr)(
    ((cfg.pgIdxBits + cfg.pgLevelBits).min(cfg.paddrBits)) - 1,
    cfg.idxLSB
  ) === 0.U
  val block_probe_for_ordering = releaseInFlight || block_probe_for_pending_release_ack || grantInProgress
  metaArb.io.in(6).valid := tl_out.b.valid && (!block_probe_for_core_progress || lrscBackingOff)
  tl_out.b.ready := metaArb.io
    .in(6)
    .ready && !(block_probe_for_core_progress || block_probe_for_ordering || s1_valid || s2_valid)
  metaArb.io.in(6).bits.write := false.B
  metaArb.io.in(6).bits.idx := probeIdx(tl_out.b.bits)
  metaArb.io.in(6).bits.addr := Cat(io.cpu.req.bits.addr >> cfg.paddrBits, tl_out.b.bits.address)
  metaArb.io.in(6).bits.way_en := metaArb.io.in(4).bits.way_en
  metaArb.io.in(6).bits.data := metaArb.io.in(4).bits.data

  // replacement policy
  s1_victim_way := (if (replacer.perSet && cfg.nWays > 1) {
                      val repl_array = Mem(cfg.nSets, UInt(replacer.nBits.W))
                      val s1_repl_idx = s1_req.addr(cfg.idxBits + cfg.blockOffBits - 1, cfg.blockOffBits)
                      val s2_repl_idx = s2_vaddr(cfg.idxBits + cfg.blockOffBits - 1, cfg.blockOffBits)
                      val s2_repl_state = Reg(UInt(replacer.nBits.W))
                      val s2_new_repl_state = replacer.get_next_state(s2_repl_state, OHToUInt(s2_hit_way))
                      val s2_repl_wen = s2_valid_masked && s2_hit_way.orR && s2_repl_state =/= s2_new_repl_state
                      val s1_repl_state =
                        Mux(s2_repl_wen && s2_repl_idx === s1_repl_idx, s2_new_repl_state, repl_array(s1_repl_idx))
                      when(s1_valid_not_nacked) { s2_repl_state := s1_repl_state }

                      val waddr = Mux(resetting, flushCounter(cfg.idxBits - 1, 0), s2_repl_idx)
                      val wdata = Mux(resetting, 0.U, s2_new_repl_state)
                      val wen = resetting || s2_repl_wen
                      when(wen) { repl_array(waddr) := wdata }

                      replacer.get_replace_way(s1_repl_state)
                    } else {
                      replacer.way
                    })

  // release
  val (c_first, c_last, releaseDone, c_count) = edge.count(tl_out_c)
  val releaseRejected = Wire(Bool())
  val s1_release_data_valid = RegNext(dataArb.io.in(2).fire)
  val s2_release_data_valid = RegNext(s1_release_data_valid && !releaseRejected)
  releaseRejected := s2_release_data_valid && !tl_out_c.fire
  val releaseDataBeat =
    Cat(0.U, c_count) + Mux(releaseRejected, 0.U, s1_release_data_valid + Cat(0.U, s2_release_data_valid))

  val nackResponseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = TLPermissions.NtoN)
  val cleanReleaseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = s2_report_param)
  val dirtyReleaseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = s2_report_param, data = 0.U)

  tl_out_c.valid := (s2_release_data_valid || (!cfg.silentDrop.B && release_state === s_voluntary_release)) && !(c_first && release_ack_wait)
  tl_out_c.bits := nackResponseMessage
  val newCoh = WireDefault(probeNewCoh)
  releaseWay := s2_probe_way

  when(s2_victimize) {
    assert(s2_valid_flush_line || s2_flush_valid || io.cpu.s2_nack)
    val discard_line = s2_valid_flush_line && s2_req.size(1) || s2_flush_valid && flushing_req.size(1)
    release_state := Mux(
      s2_victim_dirty && !discard_line,
      s_voluntary_writeback,
      Mux(
        !cfg.silentDrop.B && !release_ack_wait && release_queue_empty && s2_victim_state
          .isValid() && (s2_valid_flush_line || s2_flush_valid || s2_readwrite && !s2_hit_valid),
        s_voluntary_release,
        s_voluntary_write_meta
      )
    )
    probe_bits := addressToProbe(s2_vaddr, Cat(s2_victim_tag, s2_req.addr(tagLSB - 1, cfg.idxLSB)) << cfg.idxLSB)
  }
  when(s2_probe) {
    val probeNack = WireDefault(true.B)
    when(s2_meta_error) {
      release_state := s_probe_retry
    }.elsewhen(s2_prb_ack_data) {
      release_state := s_probe_rep_dirty
    }.elsewhen(s2_probe_state.isValid()) {
      tl_out_c.valid := true.B
      tl_out_c.bits := cleanReleaseMessage
      release_state := Mux(releaseDone, s_probe_write_meta, s_probe_rep_clean)
    }.otherwise {
      tl_out_c.valid := true.B
      probeNack := !releaseDone
      release_state := Mux(releaseDone, s_ready, s_probe_rep_miss)
    }
    when(probeNack) { s1_nack := true.B }
  }
  when(release_state === s_probe_retry) {
    metaArb.io.in(6).valid := true.B
    metaArb.io.in(6).bits.idx := probeIdx(probe_bits)
    metaArb.io.in(6).bits.addr := Cat(io.cpu.req.bits.addr >> cfg.paddrBits, probe_bits.address)
    when(metaArb.io.in(6).ready) {
      release_state := s_ready
      s1_probe := true.B
    }
  }
  when(release_state === s_probe_rep_miss) {
    tl_out_c.valid := true.B
    when(releaseDone) { release_state := s_ready }
  }
  when(release_state === s_probe_rep_clean) {
    tl_out_c.valid := true.B
    tl_out_c.bits := cleanReleaseMessage
    when(releaseDone) { release_state := s_probe_write_meta }
  }
  when(release_state === s_probe_rep_dirty) {
    tl_out_c.bits := dirtyReleaseMessage
    when(releaseDone) { release_state := s_probe_write_meta }
  }
  when(release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta, s_voluntary_release)) {
    when(release_state === s_voluntary_release) {
      tl_out_c.bits := edge
        .Release(
          fromSource = 0.U,
          toAddress = 0.U,
          lgSize = cfg.lgCacheBlockBytes.U,
          shrinkPermissions = s2_shrink_param
        )
        ._2
    }.otherwise {
      tl_out_c.bits := edge
        .Release(
          fromSource = 0.U,
          toAddress = 0.U,
          lgSize = cfg.lgCacheBlockBytes.U,
          shrinkPermissions = s2_shrink_param,
          data = 0.U
        )
        ._2
    }
    newCoh := voluntaryNewCoh
    releaseWay := s2_victim_or_hit_way
    when(releaseDone) { release_state := s_voluntary_write_meta }
    when(tl_out_c.fire && c_first) {
      release_ack_wait := true.B
      release_ack_addr := probe_bits.address
    }
  }
  tl_out_c.bits.source := probe_bits.source
  tl_out_c.bits.address := probe_bits.address
  tl_out_c.bits.data := s2_data_corrected
  tl_out_c.bits.corrupt := inWriteback && s2_data_error_uncorrectable

  tl_out_c.bits.user.lift(AMBAProt).foreach { x =>
    x.fetch := false.B
    x.secure := true.B
    x.privileged := true.B
    x.bufferable := true.B
    x.modifiable := true.B
    x.readalloc := true.B
    x.writealloc := true.B
  }

  dataArb.io.in(2).valid := inWriteback && releaseDataBeat < cfg.refillCycles.U
  dataArb.io.in(2).bits := dataArb.io.in(1).bits
  dataArb.io.in(2).bits.write := false.B
  dataArb.io.in(2).bits.addr := (probeIdx(probe_bits) << cfg.blockOffBits) | (releaseDataBeat(
    log2Up(cfg.refillCycles) - 1,
    0
  ) << cfg.rowOffBits)
  dataArb.io.in(2).bits.wordMask := ~0.U((cfg.rowBytes / cfg.subWordBytes).W)
  dataArb.io.in(2).bits.eccMask := ~0.U((cfg.wordBytes / cfg.eccBytes).W)
  dataArb.io.in(2).bits.way_en := ~0.U(cfg.nWays.W)

  metaArb.io.in(4).valid := release_state.isOneOf(s_voluntary_write_meta, s_probe_write_meta)
  metaArb.io.in(4).bits.write := true.B
  metaArb.io.in(4).bits.way_en := releaseWay
  metaArb.io.in(4).bits.idx := probeIdx(probe_bits)
  metaArb.io.in(4).bits.addr := Cat(io.cpu.req.bits.addr >> cfg.untagBits, probe_bits.address(cfg.idxMSB, 0))
  metaArb.io.in(4).bits.data := tECC.encode(L1Metadata(cfg, tl_out_c.bits.address >> tagLSB, newCoh).asUInt)
  when(metaArb.io.in(4).fire) {
    release_state := s_ready
  }

  // cached response
  (io.cpu.resp.bits: Data).waiveAll :<>= (s2_req: Data).waiveAll
  io.cpu.resp.bits.has_data := s2_read
  io.cpu.resp.bits.replay := false.B
  io.cpu.s2_uncached := false.B
  io.cpu.s2_paddr := s2_req.addr
  // io.cpu.s2_gpa := s2_tlb_xcpt.gpa
  // io.cpu.s2_gpa_is_pte := s2_tlb_xcpt.gpa_is_pte

  // report whether there are any outstanding accesses.  disregard any
  // slave-port accesses, since they don't affect local memory ordering.
  val s1_isSlavePortAccess = s1_req.no_xcpt
  val s2_isSlavePortAccess = s2_req.no_xcpt
  io.cpu.ordered := !(s1_valid && !s1_isSlavePortAccess || s2_valid && !s2_isSlavePortAccess || cached_grant_wait)

  val s2_xcpt = Wire(chiselTypeOf(io.cpu.s2_xcpt))
  s2_xcpt.ma.ld := s2_tlb_xcpt.ma.ld
  s2_xcpt.ma.st := s2_tlb_xcpt.ma.st
  s2_xcpt.pf.ld := s2_tlb_xcpt.pf.ld
  s2_xcpt.pf.st := s2_tlb_xcpt.pf.st
  s2_xcpt.gf.ld := false.B
  s2_xcpt.gf.st := false.B
  s2_xcpt.ae.ld := s2_tlb_xcpt.ae.ld
  s2_xcpt.ae.st := s2_tlb_xcpt.ae.st

  val s1_xcpt_valid = tlb.io.req.valid && !s1_isSlavePortAccess && !s1_nack
  io.cpu.s2_xcpt := Mux(RegNext(s1_xcpt_valid), s2_xcpt, 0.U.asTypeOf(s2_xcpt))

  // uncached response
  // val s1_uncached_data_word = {
  //   val word_idx = uncachedResp.addr.extract(log2Up(rowBits / 8) - 1, log2Up(wordBytes))
  //   val words = tl_out.d.bits.data.grouped(wordBits)
  //   words(word_idx)
  // }
  // val s2_uncached_data_word = RegEnable(s1_uncached_data_word, io.cpu.replay_next)
  // val doUncachedResp = RegNext(io.cpu.replay_next)
  io.cpu.resp.valid := (s2_valid_hit_pre_data_ecc) && !s2_data_error
  io.cpu.replay_next := false.B

  // load data subword mux/sign extension
  val s2_data_word =
    (0 until cfg.rowBits by cfg.wordBits).map(i => s2_data_uncorrected(cfg.wordBits + i - 1, i)).reduce(_ | _)
  val s2_data_word_corrected =
    (0 until cfg.rowBits by cfg.wordBits).map(i => s2_data_corrected(cfg.wordBits + i - 1, i)).reduce(_ | _)
  val s2_data_word_possibly_uncached = s2_data_word
  //  Mux(cfg.pipelineWayMux.B , s2_uncached_data_word, 0.U) | s2_data_word
  val loadgen =
    new LoadGen(s2_req.size, s2_req.signed, s2_req.addr, s2_data_word_possibly_uncached, s2_sc, cfg.wordBytes)
  io.cpu.resp.bits.data := loadgen.data | s2_sc_fail
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.data_raw := s2_data_word
  io.cpu.resp.bits.store_data := pstore1_data

  // AMOs
  if (usingRMW) {
    val amoalus = (0 until cfg.dataBits / cfg.xLen).map { i =>
      val amoalu = Module(new AMOALU(cfg.xLen))
      amoalu.io.mask := pstore1_mask >> (i * (cfg.xLen / 8))
      amoalu.io.cmd := (if (cfg.usingAtomicsInCache) pstore1_cmd else M_XWR)
      amoalu.io.lhs := s2_data_word >> (i * cfg.xLen)
      amoalu.io.rhs := pstore1_data >> (i * cfg.xLen)
      amoalu
    }
    pstore1_storegen_data := amoalus.map(_.io.out).asUInt
  } else if (!cfg.usingAtomics) {
    assert(!(s1_valid_masked && s1_read && s1_write), "unsupported D$ operation")
  }

  // flushes
  when(RegNext(reset.asBool)) { resetting := true.B }
  val flushCounterNext = flushCounter +& 1.U
  val flushDone = (flushCounterNext >> log2Ceil(cfg.nSets)) === cfg.nWays.U
  val flushCounterWrap = flushCounterNext(log2Ceil(cfg.nSets) - 1, 0)
  ccover(
    s2_valid_masked && s2_cmd_flush_all && s2_meta_error,
    "TAG_ECC_ERROR_DURING_FENCE_I",
    "D$ ECC error in tag array during cache flush"
  )
  ccover(
    s2_valid_masked && s2_cmd_flush_all && s2_data_error,
    "DATA_ECC_ERROR_DURING_FENCE_I",
    "D$ ECC error in data array during cache flush"
  )
  s1_flush_valid := metaArb.io
    .in(5)
    .fire && !s1_flush_valid && !s2_flush_valid_pre_tag_ecc && release_state === s_ready && !release_ack_wait
  metaArb.io.in(5).valid := flushing && !flushed
  metaArb.io.in(5).bits.write := false.B
  metaArb.io.in(5).bits.idx := flushCounter(cfg.idxBits - 1, 0)
  metaArb.io.in(5).bits.addr := Cat(
    io.cpu.req.bits.addr >> cfg.untagBits,
    metaArb.io.in(5).bits.idx << cfg.blockOffBits
  )
  metaArb.io.in(5).bits.way_en := metaArb.io.in(4).bits.way_en
  metaArb.io.in(5).bits.data := metaArb.io.in(4).bits.data

  // Only flush D$ on FENCE.I if some cached executable regions are untracked.
  if (supports_flush) {
    when(s2_valid_masked && s2_cmd_flush_all) {
      when(!flushed && !io.cpu.s2_kill && !release_ack_wait) {
        flushing := true.B
        flushing_req := s2_req
      }
    }

    when(tl_out_a.fire) { flushed := false.B }
    when(flushing) {
      s1_victim_way := flushCounter >> log2Up(cfg.nSets)
      when(s2_flush_valid) {
        flushCounter := flushCounterNext
        when(flushDone) {
          flushed := true.B
          if (!isPow2(cfg.nWays)) flushCounter := flushCounterWrap
        }
      }
      when(flushed && release_state === s_ready && !release_ack_wait) {
        flushing := false.B
      }
    }
  }
  metaArb.io.in(0).valid := resetting
  metaArb.io.in(0).bits := metaArb.io.in(5).bits
  metaArb.io.in(0).bits.write := true.B
  metaArb.io.in(0).bits.way_en := ~0.U(cfg.nWays.W)
  metaArb.io.in(0).bits.data := tECC.encode(L1Metadata(cfg, 0.U, ClientMetadata.onReset).asUInt)
  when(resetting) {
    flushCounter := flushCounterNext
    when(flushDone) {
      resetting := false.B
      if (!isPow2(cfg.nWays)) flushCounter := flushCounterWrap
    }
  }

  // report errors
  val (data_error, data_error_uncorrectable, data_error_addr) = {
    (
      RegNext(tl_out_c.fire && inWriteback && s2_data_error),
      RegNext(s2_data_error_uncorrectable),
      probe_bits.address
    ) // This is stable for a cycle after tl_out_c.fire, so don't need a register
  }
  // {
  //   val error_addr =
  //     Mux(
  //       metaArb.io.in(1).valid,
  //       Cat(s2_first_meta_corrected.tag, metaArb.io.in(1).bits.addr(tagLSB - 1, cfg.idxLSB)),
  //       data_error_addr >> cfg.idxLSB
  //     ) << cfg.idxLSB
  //   io.errors.uncorrectable.foreach { u =>
  //     u.valid := metaArb.io.in(1).valid && s2_meta_error_uncorrectable || data_error && data_error_uncorrectable
  //     u.bits := error_addr
  //   }
  //   io.errors.correctable.foreach { c =>
  //     c.valid := metaArb.io.in(1).valid || data_error
  //     c.bits := error_addr
  //     io.errors.uncorrectable.foreach { u => when(u.valid) { c.valid := false.B } }
  //   }
  //   io.errors.bus.valid := tl_out.d.fire && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
  //   io.errors.bus.bits := Mux(grantIsCached, s2_req.addr >> cfg.idxLSB << cfg.idxLSB, 0.U)

  //   ccoverNotScratchpad(io.errors.bus.valid && grantIsCached, "D_ERROR_CACHED", "D$ D-channel error, cached")
  //   ccover(io.errors.bus.valid && !grantIsCached, "D_ERROR_UNCACHED", "D$ D-channel error, uncached")
  // }

  def encodeData(x: UInt, poison: Bool) =
    x.grouped(cfg.eccBits).map(dECC.encode(_, if (dECC.canDetect) poison else false.B)).asUInt
  def dummyEncodeData(x:    UInt) = x.grouped(cfg.eccBits).map(dECC.swizzle(_)).asUInt
  def decodeData(x:         UInt) = x.grouped(dECC.width(cfg.eccBits)).map(dECC.decode(_))
  def eccMask(byteMask:     UInt) = byteMask.grouped(cfg.eccBytes).map(_.orR).asUInt
  def eccByteMask(byteMask: UInt) = FillInterleaved(cfg.eccBytes, eccMask(byteMask))

  def likelyNeedsRead(req: CacheReq) = {
    val res = !req.cmd.isOneOf(M_XWR, M_PFW) || req.size < log2Ceil(cfg.eccBytes).U
    assert(!needsRead(req) || res)
    res
  }
  def needsRead(req: CacheReq) =
    isRead(req.cmd) ||
      (isWrite(req.cmd) && (req.cmd === M_PWR || req.size < log2Ceil(cfg.eccBytes).U))

  def ccover(
    cond:  Bool,
    label: String,
    desc:  String
  )(
    implicit sourceInfo: SourceInfo
  ) =
    property.cover(cond, s"DCACHE_$label", "MemorySystem;;" + desc)

  def tagLSB: Int = cfg.untagBits
  def probeIdx(b: TLBundleB): UInt = b.address(cfg.idxMSB, cfg.idxLSB)
  def addressToProbe(vaddr: UInt, paddr: UInt): TLBundleB = {
    val res = Wire(new TLBundleB(edge.bundle))
    res :#= DontCare
    res.address := paddr
    res.source := 0.U
    res
  }
  def acquire(vaddr: UInt, paddr: UInt, param: UInt): TLBundleA = {
    if (!edge.manager.anySupportAcquireB) WireDefault(0.U.asTypeOf(new TLBundleA(edge.bundle)))
    else
      edge.AcquireBlock(0.U, paddr >> cfg.lgCacheBlockBytes << cfg.lgCacheBlockBytes, cfg.lgCacheBlockBytes.U, param)._2
  }
}
