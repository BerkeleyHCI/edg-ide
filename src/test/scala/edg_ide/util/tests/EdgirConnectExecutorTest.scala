package edg_ide.util.tests

import edgir.elem.elem
import edg.ElemBuilder._
import edg.ExprBuilder.{Ref, ValInit}
import edg.wir.ProtoUtil.{ConstraintProtoToSeqMap, LinkProtoToSeqMap}
import edg_ide.util.{BlockConnectedAnalysis, ConnectBuilder, EdgirConnectExecutor, PortConnectTyped, PortConnects}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.SeqMap

class EdgirConnectExecutorTest extends AnyFlatSpec {
  behavior.of("EdgirConnectExecutor")

  private val connectBuilderTest = new ConnectBuilderTest() // for shared examples

  val emptyBlock = Block.Block( // basic skeletal and structural example of a block with various connects
    "topDesign",
    ports = SeqMap(
      "port" -> Port.Port("sourcePort", params = SeqMap("param" -> ValInit.Integer)),
    ),
    blocks = SeqMap(
      "source" -> Block.Block(
        "sourceBlock",
        ports = SeqMap(
          "port" -> Port.Port("sourcePort", params = SeqMap("param" -> ValInit.Integer)),
        ),
      ),
      "sink0" -> Block.Block(
        "sinkBlock",
        ports = SeqMap(
          "port" -> Port.Port("sinkPort", params = SeqMap("param" -> ValInit.Integer)),
        ),
      ),
      "sink1" -> Block.Block(
        "sinkBlock",
        ports = SeqMap(
          "port" -> Port.Port("sinkPort", params = SeqMap("param" -> ValInit.Integer)),
        ),
      ),
      "sinkArray" -> Block.Block(
        "sinkArrayBlock",
        ports = SeqMap(
          "port" -> Port.Array(
            "sinkPort",
            Seq("0"),
            Port.Port("sinkPort", params = SeqMap("param" -> ValInit.Integer))
          ),
        ),
      ),
    ),
  ).getHierarchy

  it should "create a new link, source first" in {
    val startingPort = PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort"))
    val newConnects = Seq(
      PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockPort("sink1", "port"), LibraryPath("sinkPort")),
    )
    val newConnected = ConnectBuilder(emptyBlock, connectBuilderTest.exampleLink, Seq()).get
      .append(startingPort +: newConnects).get
    val connected = EdgirConnectExecutor(emptyBlock, None, newConnected, startingPort, newConnects).get
    connected.links.toSeqMap should equal(SeqMap(
      "_new" -> elem.LinkLike(elem.LinkLike.Type.Link(connectBuilderTest.exampleLink))
    ))
    connected.constraints.toSeqMap should equal(SeqMap(
      "_new2" -> Constraint.Connected(Ref("source", "port"), Ref("_new", "source")),
      "_new3" -> Constraint.Connected(Ref("sink0", "port"), Ref("_new", "sinks")), // TODO should be allocate
      "_new4" -> Constraint.Connected(Ref("sink1", "port"), Ref("_new", "sinks")), // TODO should be allocate
    ))
  }

  it should "create a new link, sink first" in {
    val startingPort = PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort"))
    val newConnects = Seq(
      PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("sink1", "port"), LibraryPath("sinkPort")),
    )
    val newConnected = ConnectBuilder(emptyBlock, connectBuilderTest.exampleLink, Seq()).get
      .append(startingPort +: newConnects).get
    val connected = EdgirConnectExecutor(emptyBlock, None, newConnected, startingPort, newConnects).get
    connected.links.toSeqMap should equal(SeqMap(
      "_new" -> elem.LinkLike(elem.LinkLike.Type.Link(connectBuilderTest.exampleLink))
    ))
    connected.constraints.toSeqMap should equal(SeqMap(
      "_new2" -> Constraint.Connected(Ref("sink0", "port"), Ref("_new", "sinks")), // TODO should be allocate
      "_new3" -> Constraint.Connected(Ref("source", "port"), Ref("_new", "source")),
      "_new4" -> Constraint.Connected(Ref("sink1", "port"), Ref("_new", "sinks")), // TODO should be allocate
    ))
  }
}
