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
    analysis.connectedGroups.keys should equal(Set("link", "port", "bundle", "unusedSink.port"))
    analysis.connectedGroups("link")._1 should equal(Seq(
      PortConnectTyped(PortConnects.BlockPort("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockPort("sink1", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorSlicePort("sinkArray", "port", None), LibraryPath("sinkPort")),
    ))
    analysis.connectedGroups("link")._2 should not be empty

    analysis.connectedGroups("port")._1 should equal(Seq( // export
      PortConnectTyped(PortConnects.BoundaryPort("port", Seq()), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("exportSource", "port"), LibraryPath("sourcePort")),
    ))
    analysis.connectedGroups("port")._2 should not be empty

    analysis.connectedGroups("bundle")._1 should equal(Seq( // export into bundle elt
      PortConnectTyped(PortConnects.BoundaryPort("bundle", Seq("port")), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockPort("exportBundleSource", "port"), LibraryPath("sourcePort")),
    ))
    analysis.connectedGroups("bundle")._2 should not be empty

    analysis.connectedGroups("unusedSink.port")._1 should equal(Seq( // export into bundle elt
      PortConnectTyped(PortConnects.BlockPort("unusedSink", "port"), LibraryPath("sinkPort"))))
    analysis.connectedGroups("unusedSink.port")._2 shouldBe empty
  }

  it should "decode connections for array example" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleArrayBlock)
    analysis.connectedGroups.keys should equal(Set("link", "unusedSinkArray.port"))
    analysis.connectedGroups("link")._1 should equal(Seq(
      PortConnectTyped(PortConnects.BlockVectorUnit("source", "port"), LibraryPath("sourcePort")),
      PortConnectTyped(PortConnects.BlockVectorUnit("sink0", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorUnit("sink1", "port"), LibraryPath("sinkPort")),
      PortConnectTyped(PortConnects.BlockVectorSliceVector("sinkArray", "port", None), LibraryPath("sinkPort"))
    ))
    analysis.connectedGroups("link")._2 should not be empty

    analysis.connectedGroups("unusedSinkArray.port")._1 should equal(Seq( // export into bundle elt
      PortConnectTyped(PortConnects.BlockVectorUnit("unusedSinkArray", "port"), LibraryPath("sinkPort"))))
    analysis.connectedGroups("unusedSinkArray.port")._2 shouldBe empty
  }
}
