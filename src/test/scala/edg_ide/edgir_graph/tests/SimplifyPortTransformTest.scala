package edg_ide.edgir_graph.tests

import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edg_ide.edgir_graph.{EdgirGraph, PruneArrayPortsTransform, SimplifyPortTransform}

import scala.collection.SeqMap


class SimplifyPortTransformTest extends AnyFlatSpec with Matchers {
  behavior of "SimplifyPortTransform"

  it should "work on a flat source-to-sink design" in {
    val testGraph = EdgirGraph.EdgirNode(
      data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath()),
      members = SeqMap(
        "block" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "block"),
          members = SeqMap(
            "conArray[0]" -> EdgirGraph.EdgirPort(  // port 0 is connected
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "block" + "conArray[0]")
            ),
            "conArray[1]" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "conArray[1]")
            ),
            "conArray[2]" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "conArray[2]")
            ),
            "conArray1[0]" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "conArray1[0]")
            ),
            "conArray1[1]" -> EdgirGraph.EdgirPort(  // port 1 is connected, but port 0 should show up
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "conArray1[1]")
            ),
            "conArray1[2]" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "conArray1[2]")
            ),
            "unconArray[0]" -> EdgirGraph.EdgirPort(  // nothing connected, but retain port 0
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "unconArray[0]")
            ),
            "unconArray[1]" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "unconArray[1]")
            ),
            "unconArray[2]" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "unconArray[2]")
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "conArray"),
          source = Seq("block", "conArray[0]"),
          target = Seq("link", "whatevs")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "conArray1"),
          source = Seq("block", "conArray1[1]"),
          target = Seq("link", "whatevs")
        ),
      )
    )

    val transformed = PruneArrayPortsTransform(testGraph)

    transformed.members("block").asInstanceOf[EdgirNode].members.keySet should equal(Set(
      "conArray[0]", "conArray[1]",
      "conArray1[0]", "conArray1[1]",
      "unconArray[0]"
    ))
  }
}
