package edg_ide.edgir_graph.tests

import edg_ide.edgir_graph.EdgirGraph.EdgirNode
import edg_ide.edgir_graph.{EdgirGraph, CollapseNodeTransform, ConnectWrapper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CollapseNodeTransformTest extends AnyFlatSpec with Matchers {
  behavior of "CollapseNodeTransform"

  it should "collapse nodes and replace edges" in {
    val testGraph = EdgirGraph.EdgirNode(
      data = EdgirTestUtils.Dummy.BlockWrapper,
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper,
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper,
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper,
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper("connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(  // edge directions properly inferred
          data = EdgirTestUtils.Dummy.ConnectWrapper("connect_sink"),
          source = Seq("link", "sinks"),
          target = Seq("sink", "port")
        ),
      )
    )

    val collapse = new CollapseNodeTransform {}
    val transformed = collapse.collapse(testGraph, "link",
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
