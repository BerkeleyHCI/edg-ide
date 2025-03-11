package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.{EdgirGraph, GroupingTransform, InferEdgeDirectionTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap

class GroupingTransformTest extends AnyFlatSpec with Matchers {
  behavior.of("GroupingTransform")

  it should "no-op" in {
    val transformed = GroupingTransform(
      InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      SeqMap()
    )

    transformed should equal(InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph))
  }

  it should "group nodes including internal edges" in {
    val transformed = GroupingTransform(
      InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      SeqMap("group" -> Seq("source", "sink", "link"))
    )

    transformed.members should equal(
      SeqMap(Seq("group") -> InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph))
    )
    transformed.edges should equal(Seq())
  }

  it should "group nodes and convert inter-group edges to degenerate edges" in {
    val transformed = GroupingTransform(
      InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      SeqMap("src_group" -> Seq("source", "link"), "snk_group" -> Seq("sink"))
    )

    transformed.members(Seq("src_group")).asInstanceOf[EdgirGraph.EdgirNode].members.keys.toSeq should equal(
      Seq(Seq("source"), Seq("link"))
    )
    transformed.members(Seq("snk_group")).asInstanceOf[EdgirGraph.EdgirNode].members.keys.toSeq should equal(
      Seq(Seq("sink"))
    )
    transformed.members(Seq("src_group")).asInstanceOf[EdgirGraph.EdgirNode].edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.TestGraphs.flatGraph.edges(0).data, // internal link for two blocks within group
        source = Seq("source", "port"),
        target = Seq("link", "source")
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.TestGraphs.flatGraph.edges(1).data, // degenerate edge for connection to other group
        source = Seq("link", "sinks", "0"),
        target = Seq("link", "sinks", "0")
      )
    ))
    transformed.members(Seq("snk_group")).asInstanceOf[EdgirGraph.EdgirNode].edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.TestGraphs.flatGraph.edges(1).data,
        source = Seq("sink", "port"),
        target = Seq("sink", "port")
      )
    ))
    transformed.edges should equal(Seq()) // no edges left at top level
  }
}
