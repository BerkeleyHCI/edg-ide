package edg_ide.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConverters._
import org.eclipse.elk.graph._
import org.eclipse.elk.graph.util.ElkGraphUtil
import edg_ide.{HierarchyGraphElk, HGraphNodeMember, HGraphNode, HGraphPort, HGraphEdge}

class HierarchyGraphElkTest extends AnyFlatSpec with Matchers {
  type SimpleNodeMember = HGraphNodeMember[Nothing, Nothing, Nothing]
  type SimpleNode = HGraphNode[Nothing, Nothing, Nothing]
  val SimpleNode = HGraphNode
  type SimplePort = HGraphPort[Nothing]
  val SimplePort = HGraphPort
  type SimpleEdge = HGraphEdge[Nothing]
  val SimpleEdge = HGraphEdge

  behavior of "HGraphNodeToElkNode"

  def makeGraph(): (ElkNode, Map[Seq[String], ElkConnectableShape]) = {
    val simpleGraph = SimpleNode(None,
      members = Map(
        "p1" -> SimplePort(None),
        "n1" -> SimpleNode(None,
          members = Map(
            "n1p1" -> SimplePort(None),
            "n1p2" -> SimplePort(None),
          ),
          edges = Seq()
        ),
        "n2" -> SimpleNode(None,
          members = Map(
            "n2p1" -> SimplePort(None),
          ),
          edges = Seq()
        )
      ), edges = Seq(
        SimpleEdge(None, source=Seq("p1"), target=Seq("n1", "n1p1")),
        SimpleEdge(None, source=Seq("n1", "n1p1"), target=Seq("n2", "n2p1")),
      )
    )
    val root = ElkGraphUtil.createGraph()
    val elkElementsMap = HierarchyGraphElk.HGraphNodeToElkNode(simpleGraph, "", root)

    (root, elkElementsMap)
  }

  it should "return a map of internal elements" in {
    val (_, elkElementsMap) = makeGraph()

    elkElementsMap should contain key Seq("p1")
    elkElementsMap(Seq("p1")) shouldBe a[ElkPort]
  }

  it should "create ELK elements" in {
    val (root, _) = makeGraph()

    root.getChildren.asScala should have size 1
    val graphNode = root.getChildren.asScala.head
    graphNode.getChildren.asScala should have size 2
    graphNode.getPorts.asScala should have size 1
    graphNode.getContainedEdges.asScala should have size 2
  }
}
