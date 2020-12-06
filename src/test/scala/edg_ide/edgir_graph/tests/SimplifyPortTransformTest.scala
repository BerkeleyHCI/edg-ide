package edg_ide.edgir_graph.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edg_ide.edgir_graph.{EdgirGraph, SimplifyPortTransform}


class SimplifyPortTransformTest extends AnyFlatSpec with Matchers {
  behavior of "SimpliftPortTransform"

  it should "work on a flat source-to-sink design" in {
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
            "sink" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = "connect_source",
          source = Seq("source", "port", "subport"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = "connect_sink",
          source = Seq("sink", "port"),
          target = Seq("link", "sink", "subport")
        ),
      )
    )

    val transformed = SimplifyPortTransform(testGraph)

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = "connect_source",
        source = Seq("source", "port"),
        target = Seq("link", "source")
      ),
      EdgirGraph.EdgirEdge(
        data = "connect_sink",
        source = Seq("sink", "port"),
        target = Seq("link", "sink")
      ),
    ))
  }

}
