package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.InferEdgeDirectionTransform
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap

class GroupingTransformTest extends AnyFlatSpec with Matchers {
  behavior.of("GroupingTransform")

  it should "group nodes including internal edges" in {
    val grouper = new GrouperTransform {}
    val transformed = grouper(
      InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      SeqMap("group" -> Seq("source", "sink"))
    )

    transformed.members should equal(
      SeqMap("group" -> InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph))
    )
    transformed.edges should equal(Seq())
  }

  it should "group nodes and convert inter-group edges to degenerate edges" in {
    val grouper = new GrouperTransform {}
    val transformed = grouper(
      InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      SeqMap("src_group" -> Seq("source"), "snk_group" -> Seq("sink"))
    )

    transformed.members("src_group").members.keys.toSeq should equal(Seq("source"))
    transformed.members("snk_group").members.keys.toSeq should equal(Seq("sink"))
    print(transformed.members("snk_group").edges) // TODO add reference degenerate edges
    transformed.edges should equal(Seq()) // should be no top edges
  }
}
