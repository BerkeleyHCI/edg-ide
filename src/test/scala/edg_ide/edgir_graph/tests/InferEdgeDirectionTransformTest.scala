package edg_ide.edgir_graph.tests

import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirNode
import edg_ide.edgir_graph.{EdgirGraph, InferEdgeDirectionTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class InferEdgeDirectionTransformTest extends AnyFlatSpec with Matchers {
  behavior of "InferEdgeDirectionTransform"

  it should "properly direction flat graphs" in {
    val transformed = InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph)

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_source"),
        source = Seq("source", "port"),
        target = Seq("link", "source")
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_sink"),
        source = Seq("link", "sinks"),
        target = Seq("sink", "port")
      ),
    ))
  }

  it should "properly direction hierarchical exports" in {
    val transformed = InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.hierarchyGraph)

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_source"),
        source = Seq("source", "port"),
        target = Seq("link", "source")
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_sink"),
        source = Seq("link", "sinks"),
        target = Seq("sink", "port")
      ),
    ))

    transformed.members("source").asInstanceOf[EdgirNode].edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "source" + "export_inner"),
        source = Seq("inner", "port"),
        target = Seq("port")
      )
    ))
    transformed.members("sink").asInstanceOf[EdgirNode].edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "sink" + "export_inner"),
        source = Seq("port"),
        target = Seq("inner", "port")
      )
    ))
  }
}
