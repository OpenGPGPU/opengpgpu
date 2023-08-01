import chisel3._
import chisel3.util.log2Ceil
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import chisel3.util.random.FibonacciLFSR
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.ElaborationArtefacts
import org.chipsalliance.cde.config._
import java.io.{File, FileWriter}

case class UpwardParam()
case class DownwardParam(width: Int)
case class EdgeParam(width: Int)

case object AdderInputNum extends Field[Int]
case object BitWidth extends Field[Int]

object AdderNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {
  override def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) = {
    EdgeParam(pd.width)
  }

  override def bundle(e: EdgeParam) = UInt(e.width.W)

  override def render(e: EdgeParam) = RenderedEdge("#cc00cc", s"${e.width}")
}

case class AdderDriverNode(width: Int)(implicit v: ValName) extends SourceNode(AdderNodeImp)(Seq(DownwardParam(width)))

class AdderDriver(width: Int)(implicit p: Parameters) extends LazyModule {
  val node = AdderDriverNode(width)

  lazy val module = new LazyModuleImp(this) {
    val negotiatedWidths = node.edges.out.map(_.width)
    require(negotiatedWidths.length == 1, "Can only give one output")
    val finalWidth = negotiatedWidths.head

    val randomAddend = FibonacciLFSR.maxPeriod(finalWidth)

    node.out.foreach {
      case (addend, _) => addend := randomAddend
    }
  }
}

case class AdderJunctionNode(
  dFn: Seq[DownwardParam] => Seq[DownwardParam],
  uFn: Seq[UpwardParam] => Seq[UpwardParam]
)(
  implicit v: ValName)
    extends JunctionNode(AdderNodeImp)(dFn, uFn)

class AdderJunction(implicit p: Parameters) extends LazyModule {
  val node: AdderJunctionNode = AdderJunctionNode(
    dFn = { seq =>
      require(seq.length == 1, "Can only give one input")
      println(s"dRatio = ${node.dRatio} uRatio = ${node.uRatio}")
      Seq.fill(node.dRatio)(seq.head)
    },
    uFn = { _ => Seq(UpwardParam()) }
  )

  lazy val module = new LazyModuleImp(this) {
    println(s"numInOutGrouped = ${node.inoutGrouped.length}")
    node.inoutGrouped.foreach {
      case (in, out) =>
        println(s"numIn = ${in.length} numOut = ${out.length}")
        in.foreach { case (inPort, _) => out.foreach { case (outPort, _) => outPort := inPort } }
    }
  }
}

case class AdderNode(
  dFn: Seq[DownwardParam] => DownwardParam,
  uFn: Seq[UpwardParam] => UpwardParam
)(
  implicit v: ValName)
    extends NexusNode(AdderNodeImp)(dFn, uFn)

class ActualAdder(implicit p: Parameters) extends LazyModule {
  val node = AdderNode(
    dFn = { seq =>
      DownwardParam(seq.map(p => p.width).max + log2Ceil(seq.length))
    },
    uFn = { _ => UpwardParam() }
  )

  lazy val module = new LazyModuleImp(this) {
    val negotiatedOutputWidths = node.edges.out.map { e => e.width }
    require(negotiatedOutputWidths.forall(_ == negotiatedOutputWidths.head))
    val finalWidth = negotiatedOutputWidths.head

    val result = Reg(UInt(finalWidth.W))
    result := node.in.map(_._1).reduce(_ +& _)
    node.out.map(_._1).foreach(_ := result)
  }
}

case class AdderMonitorNode(numInput: Int)(implicit v: ValName)
    extends SinkNode(AdderNodeImp)(Seq.fill(numInput)(UpwardParam()))

class AdderMonitor(implicit p: Parameters) extends LazyModule {
  val monitorNode = AdderMonitorNode(p(AdderInputNum))
  val resultNode = AdderMonitorNode(1)
  val outputNode = BundleBridgeSource(Some(() => Output(Bool())))

  lazy val module = new LazyModuleImp(this) {
    printf(
      monitorNode.in.map(n => p"${n._1}").reduce(_ + p" + " + _) + p" = ${resultNode.in.head._1}"
    )
    val expectedResult = RegNext(monitorNode.in.map(_._1).reduce(_ +& _))
    outputNode.bundle := resultNode.in.head._1 =/= expectedResult
  }
}

class AdderTestBench(implicit p: Parameters) extends LazyModule {

  val drivers = Seq.tabulate(p(AdderInputNum)) { i =>
    LazyModule(new AdderDriver(p(BitWidth))).suggestName(s"driver_${i}")
  }
  val adder = LazyModule(new ActualAdder())
  val monitor = LazyModule(new AdderMonitor())
  val junction = LazyModule(new AdderJunction())

  val outputNode = BundleBridgeSink(Some(() => Output(Bool())))

  drivers.foreach { d => junction.node := d.node }
  monitor.resultNode := adder.node :=* junction.node
  monitor.monitorNode :=* junction.node
  outputNode := monitor.outputNode

  lazy val module = new LazyModuleImp(this) {
    // Nothing to do here
  }

  val error = InModuleBody { outputNode.makeIO("error") }
}

class Top(implicit p: Parameters) extends Module {
  val error = IO(Output(Bool()))

  private val ldut: AdderTestBench = LazyModule(new AdderTestBench())
  val dut = Module(ldut.module)

  error := ldut.error

  ElaborationArtefacts.add("graphml", ldut.graphML)
}

case object PlaygroundConfig
    extends Config((_, _, _) => {
      case AdderInputNum => 4
      case BitWidth      => 16
    })

object Playground extends App {
  implicit val p: Parameters = PlaygroundConfig

  (new ChiselStage).emitVerilog(
    new Top,
    // LazyModule(new AdderTestBench()).module,
    Array("--target-dir", "generated")
  )

  ElaborationArtefacts.files.foreach {
    case ("graphml", graphML) =>
      val fw = new FileWriter(new File("generated", "AdderTestBench.graphml"))
      fw.write(graphML())
      fw.close()
  }
}
