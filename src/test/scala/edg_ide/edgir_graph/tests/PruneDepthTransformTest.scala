package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.PruneDepthTransform
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PruneDepthTransformTest extends AnyFlatSpec with Matchers {
  behavior.of("PruneDepthTransform")

  it should "prune nodes below some depth" in {
    val transformed = PruneDepthTransform(EdgirTestUtils.TestGraphs.hierarchyGraph, 1)
    transformed should equal(EdgirTestUtils.TestGraphs.flatGraph)
  }
}
