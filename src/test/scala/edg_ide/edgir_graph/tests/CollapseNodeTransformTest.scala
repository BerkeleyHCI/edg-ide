package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.{CollapseNodeTransform, ConnectWrapper, EdgirGraph, InferEdgeDirectionTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CollapseNodeTransformTest extends AnyFlatSpec with Matchers {
  behavior of "CollapseNodeTransform"

  it should "collapse nodes and replace edges" in {
    val collapse = new CollapseNodeTransform {}
    val transformed = collapse.collapse(InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      "link",
      edges => EdgirTestUtils.Dummy.ConnectWrapper(
        edges.map{_.asInstanceOf[ConnectWrapper].name}.mkString(", ")))

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper("connect_source, connect_sink"),
        source = Seq("source", "port"),
        target = Seq("sink", "port")
      ),
    ))
    transformed.members.keys.toSeq should equal(Seq("source", "sink"))
  }
}
