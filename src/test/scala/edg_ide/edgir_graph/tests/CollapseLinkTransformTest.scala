package edg_ide.edgir_graph.tests

import edg.wir.DesignPath
import edg_ide.edgir_graph.{CollapseLinkTransform, EdgeLinkWrapper, EdgirGraph, InferEdgeDirectionTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CollapseLinkTransformTest extends AnyFlatSpec with Matchers {
  behavior of "CollapseLinkTransform"

  it should "collapse links" in {
    val transformed = CollapseLinkTransform(InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph))

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgeLinkWrapper(DesignPath() + "link", EdgirTestUtils.Dummy.LinkLike),
        source = Seq("source", "port"),
        target = Seq("sink", "port")
      ),
    ))
    transformed.members.keys.toSeq should equal(Seq("source", "sink"))
  }
}
