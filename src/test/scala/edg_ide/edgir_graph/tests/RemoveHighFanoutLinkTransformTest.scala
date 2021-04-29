package edg_ide.edgir_graph.tests

import edg.wir.DesignPath
import edg_ide.edgir_graph.{CollapseLinkTransform, EdgeLinkWrapper, EdgirGraph, InferEdgeDirectionTransform, LinkWrapper, RemoveHighFanoutLinkTransform}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import edg.ElemBuilder.{Block, Link, LibraryPath}

import scala.collection.SeqMap


class RemoveHighFanoutLinkTransformTest extends AnyFlatSpec with Matchers {
  behavior of "RemoveHighFanoutLinkTransform"

  val testGraph = EdgirGraph.EdgirNode(
    data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath()),
    members = SeqMap(
      // TODO this is kind of degenerate in that it's missing its ports, but it tests well enough
      "link" -> EdgirGraph.EdgirNode(
        data = LinkWrapper(DesignPath() + "link", Link.Link(
          selfClass="testLink"
        )),
        members = SeqMap(),
        edges = Seq()
      )
    ),
    edges = Seq(
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "link" + "source"),
        source = Seq("sourceBlock", "port"),
        target = Seq("link", "source")
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "link" + "source"),
        source = Seq("sinkBlock1", "port"),
        target = Seq("link", "sink[0]")
      ),
      EdgirGraph.EdgirEdge(
        data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "link" + "source"),
        source = Seq("sinkBlock2", "port"),
        target = Seq("link", "sink[1]")
      ),
    ))

  it should "remove high fanout links" in {
    val transformed = new RemoveHighFanoutLinkTransform(3,
      Set(LibraryPath("testLink")))(testGraph)

    transformed.members.size should equal(0)
  }

  it should "not remove on fewer than specified connects" in {
    val transformed = new RemoveHighFanoutLinkTransform(4,
      Set(LibraryPath("testLink")))(testGraph)

    transformed should equal(testGraph)
  }

  it should "not remove on link name mismatch" in {
    val transformed = new RemoveHighFanoutLinkTransform(3,
      Set(LibraryPath("notAName")))(testGraph)

    transformed should equal(testGraph)
  }
}
