package edg_ide.util.tests

import edg_ide.util.{BlockConnectedAnalysis, PortConnects}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BlockConnectedAnalysisTest extends AnyFlatSpec {
  behavior.of("BlockConnectedAnalysis")

  private val connectBuilderTest = new ConnectBuilderTest() // for shared examples

  it should "decode connections for non-array example" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleBlock)
    analysis.connectedGroups.keys should equal(Set("link", "port", "bundle", "unusedSink.port"))
    analysis.connectedGroups("link")._1 should equal(Seq(
      PortConnects.BlockPort("source", "port"),
      PortConnects.BlockPort("sink0", "port"),
      PortConnects.BlockPort("sink1", "port"),
      PortConnects.BlockVectorSlicePort("sinkArray", "port", None),
    ))
    analysis.connectedGroups("link")._2 should not be empty

    analysis.connectedGroups("port")._1 should equal(Seq( // export
      PortConnects.BoundaryPort("port", Seq()),
      PortConnects.BlockPort("exportSource", "port"),
    ))
    analysis.connectedGroups("port")._2 should not be empty

    analysis.connectedGroups("bundle")._1 should equal(Seq( // export into bundle elt
      PortConnects.BoundaryPort("bundle", Seq("port")),
      PortConnects.BlockPort("exportBundleSource", "port"),
    ))
    analysis.connectedGroups("bundle")._2 should not be empty

    analysis.connectedGroups("unusedSink.port")._1 should equal(Seq( // export into bundle elt
      PortConnects.BlockPort("unusedSink", "port")))
    analysis.connectedGroups("unusedSink.port")._2 shouldBe empty
  }

  it should "decode connections for array example" in {
    val analysis = new BlockConnectedAnalysis(connectBuilderTest.exampleArrayBlock)
    analysis.connectedGroups.keys should equal(Set("link", "unusedSinkArray.port"))
    analysis.connectedGroups("link")._1 should equal(Seq(
      PortConnects.BlockVectorUnit("source", "port"),
      PortConnects.BlockVectorUnit("sink0", "port"),
      PortConnects.BlockVectorUnit("sink1", "port"),
      PortConnects.BlockVectorSliceVector("sinkArray", "port", None),
    ))
    analysis.connectedGroups("link")._2 should not be empty

    analysis.connectedGroups("unusedSinkArray.port")._1 should equal(Seq( // export into bundle elt
      PortConnects.BlockVectorUnit("unusedSinkArray", "port")))
    analysis.connectedGroups("unusedSinkArray.port")._2 shouldBe empty
  }
}
