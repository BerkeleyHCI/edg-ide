package edg_ide.edgir_graph.tests

import edg.wir.DesignPath
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edg_ide.edgir_graph.{EdgirGraph, SimplifyPortTransform}

import scala.collection.SeqMap


class SimplifyPortTransformTest extends AnyFlatSpec with Matchers {
  behavior of "SimplifyPortTransform"

  it should "work on a flat source-to-sink design" in {
    val testGraph = EdgirGraph.EdgirNode(
      data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath()),
      members = SeqMap(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "source"),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "port")
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "sink"),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "sink" + "port")
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper(DesignPath() + "link"),
          members = SeqMap(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "link" + "source")
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "link" + "sinks")
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_source"),
          source = Seq("source", "port", "subport"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_sink"),
          source = Seq("sink", "port"),
          target = Seq("link", "sinks", "subport")
        ),
      )
    )

    val transformed = SimplifyPortTransform(testGraph)

    transformed.edges should equal(Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_source"),
        source = Seq("source", "port"),
        target = Seq("link", "source")
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_sink"),
        source = Seq("sink", "port"),
        target = Seq("link", "sinks")
      ),
    ))
  }

}
