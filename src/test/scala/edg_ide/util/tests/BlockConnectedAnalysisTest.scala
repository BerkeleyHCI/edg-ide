package edg_ide.util.tests

import edg.ElemBuilder._
import edg_ide.util.{BlockConnectedAnalysis, PortConnectTyped, PortConnects}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BlockConnectedAnalysisTest extends AnyFlatSpec {
  behavior.of("BlockConnectedAnalysis")

  private val connectBuilderTest = new ConnectBuilderTest() // for shared examples

  it should "decode connections for non-array example" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleBlock)
    analysis.connectedGroups.map(_._1) should equal(Seq(Some("link"), None, None, None, None))
    analysis.connectedGroups(0)._1 should equal(Some("link"))
    analysis.connectedGroups(0)._2 should equal(Seq(
      PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockPort("sink1", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort")),
    ))
    analysis.connectedGroups(0)._3 should not be empty

    analysis.connectedGroups(1)._1 should equal(None)
    analysis.connectedGroups(1)._2 should equal(Seq( // export
      PortConnectTyped(PortConnects.BoundaryPort("port", Seq()), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("exportSource", "port"), LibraryPath("sourcePort")),
    ))
    analysis.connectedGroups(1)._3 should not be empty

    analysis.connectedGroups(2)._1 should equal(None)
    analysis.connectedGroups(2)._2 should equal(Seq( // export into bundle elt
      PortConnectTyped(PortConnects.BoundaryPort("bundle", Seq("port")), LibraryPath("sourceBundle")),
      PortConnectTyped(PortConnects.BlockPort("exportBundleSource", "port"), LibraryPath("sourcePort")),
    ))
    analysis.connectedGroups(2)._3 should not be empty

    analysis.connectedGroups(3)._1 should equal(None)
    analysis.connectedGroups(3)._2 should equal(Seq( // array can have additional allocates
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort"))))
    analysis.connectedGroups(3)._3 shouldBe empty

    analysis.connectedGroups(4)._1 should equal(None)
    analysis.connectedGroups(4)._2 should equal(Seq(
      PortConnectTyped(PortConnects.BlockPort("unusedSink", "port"), LibraryPath("sinkPort"))
    ))
    analysis.connectedGroups(4)._3 shouldBe empty
  }

  it should "return connected group for connected ports" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleBlock)

    val sourcePortResult = analysis.findConnectConnectedGroupFor(Seq("source", "port")).get
    sourcePortResult._1 should equal(Some("link"))
    sourcePortResult._2 should equal(Seq(
      PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockPort("sink1", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort")),
    ))
    sourcePortResult._3 should not be empty
    sourcePortResult._4 should equal(
      PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort"))
    )

    val sink0PortResult = analysis.findConnectConnectedGroupFor(Seq("sink0", "port")).get
    sink0PortResult._1 should equal(Some("link"))
    sink0PortResult._2 should equal(Seq(
      PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockPort("sink1", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort")),
    ))
    sink0PortResult._3 should not be empty
    sink0PortResult._4 should equal(
      PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort"))
    )
  }

  it should "return new array connected group for connected ports" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleBlock)

    val sinkArrayPortResult = analysis.findConnectConnectedGroupFor(Seq("sinkArray", "port")).get
    sinkArrayPortResult._1 should equal(None)
    sinkArrayPortResult._2 should equal(Seq(
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort"))
    ))
    sinkArrayPortResult._3 shouldBe empty
    sinkArrayPortResult._4 should equal(
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort"))
    )
  }

  it should "decode connections for array example" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleArrayBlock)
    analysis.connectedGroups.map(_._1) should equal(Seq(Some("link"), None, None))

    analysis.connectedGroups(0)._1 should equal(Some("link"))
    analysis.connectedGroups(0)._2 should equal(Seq(
      PortConnectTyped(PortConnects.BlockVectorUnit("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockVectorUnit("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorUnit("sink1", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorSliceVector("sinkArray", "port", None), LibraryPath("sinkPort"))
    ))
    analysis.connectedGroups(0)._3 should not be empty

    analysis.connectedGroups(1)._1 should equal(None)
    analysis.connectedGroups(1)._2 should equal(Seq( // array can have additional allocates
      PortConnectTyped(PortConnects.BlockVectorSliceVector("sinkArray", "port", None), LibraryPath("sinkPort"))))
    analysis.connectedGroups(1)._3 shouldBe empty

    analysis.connectedGroups(2)._1 should equal(None)
    // TODO this should be an ambiguous BlockVectorUnit, but currently defaults to a VectorSlicePort
//    analysis.connectedGroups(2)._2 should equal(Seq(
//      PortConnectTyped(PortConnects.BlockVectorUnit("unusedSinkArray", "port"), LibraryPath("sinkPort"))
//    ))
    analysis.connectedGroups(2)._3 shouldBe empty
  }
}
