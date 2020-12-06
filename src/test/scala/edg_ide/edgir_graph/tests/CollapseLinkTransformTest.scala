package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.{EdgeLinkWrapper, EdgirGraph, InferEdgeDirectionTransform, CollapseLinkTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CollapseLinkTransformTest extends AnyFlatSpec with Matchers {
  behavior of "CollapseLinkTransform"

  it should "collapse links" in {
    val transformed = CollapseLinkTransform(InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph))

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgeLinkWrapper("link", EdgirTestUtils.Dummy.LinkWrapper.linkLike),
        source = Seq("source", "port"),
        target = Seq("sink", "port")
      ),
    ))
    transformed.members.keys.toSeq should equal(Seq("source", "sink"))
  }
}
