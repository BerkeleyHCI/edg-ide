package edg_ide.edgir_graph.tests

import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirNode
import edg_ide.edgir_graph.{EdgirGraph, InferEdgeDirectionTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InferEdgeDirectionTransformTest extends AnyFlatSpec with Matchers {
  behavior.of("InferEdgeDirectionTransform")

  it should "properly direction flat graphs" in {
    val transformed = InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.flatGraph)

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_source"),
        source = Some(Seq("source", "port")),
        target = Some(Seq("link", "source"))
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_sink"),
        source = Some(Seq("link", "sinks", "0")),
        target = Some(Seq("sink", "port"))
      ),
    ))
  }

  it should "properly direction hierarchical exports" in {
    val transformed = InferEdgeDirectionTransform(EdgirTestUtils.TestGraphs.hierarchyGraph)

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_source"),
        source = Some(Seq("source", "port")),
        target = Some(Seq("link", "source"))
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_sink"),
        source = Some(Seq("link", "sinks", "0")),
        target = Some(Seq("sink", "port"))
      ),
    ))

    transformed.members(Seq("source")).asInstanceOf[EdgirNode].edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "source" + "export_inner"),
        source = Some(Seq("inner", "port")),
        target = Some(Seq("port"))
      )
    ))
    transformed.members(Seq("sink")).asInstanceOf[EdgirNode].edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "sink" + "export_inner"),
        source = Some(Seq("port")),
        target = Some(Seq("inner", "port"))
      )
    ))
  }
}
