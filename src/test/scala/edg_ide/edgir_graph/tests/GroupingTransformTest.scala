package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.InferEdgeDirectionTransform
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap

class GroupingTransformTest extends AnyFlatSpec with Matchers {
  behavior.of("GroupingTransform")

  it should "group nodes including internal edges" in {
    val grouper = new GrouperTransform {}
    val transformed = grouper.group(
      InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph),
      SeqMap("group" -> Seq("source", "sink"))
    )

    transformed.members should equal(
      SeqMap("group" -> InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph))
    )
    transformed.edges should equal(Seq())
  }

  it should "group nodes and convert inter-group edges to degenerate edges" in {}
}
