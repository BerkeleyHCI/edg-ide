package edg_ide.edgir_graph.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConverters._
import org.eclipse.elk.graph._
import org.eclipse.elk.graph.util.ElkGraphUtil
import edg_ide.edgir_graph.{HierarchyGraphElk, HGraphEdge, HGraphNode, HGraphNodeMember, HGraphPort}


class HierarchyGraphElkTest extends AnyFlatSpec with Matchers {
  sealed trait SimpleNodeMember extends HGraphNodeMember[Null, Null, Null] {
  }

  case class SimpleNode(
    override val members: Map[String, SimpleNodeMember],
    override val edges: Seq[HGraphEdge[Null]],
  ) extends HGraphNode[Null, Null, Null] with SimpleNodeMember {
    override val data: Null = null
  }

  case class SimplePort(
  ) extends HGraphPort[Null] with SimpleNodeMember {
    override val data: Null = null
  }

  case class SimpleEdge(
    override val source: Seq[String],
    override val target: Seq[String]
  ) extends HGraphEdge[Null] {
    override val data: Null = null
  }


  behavior of "HGraphNodeToElkNode"

  def makeGraph(): (ElkNode, Map[Seq[String], ElkConnectableShape]) = {
    val simpleGraph = SimpleNode(
      members = Map(
        "p1" -> SimplePort(),
        "n1" -> SimpleNode(
          members = Map(
            "n1p1" -> SimplePort(),
            "n1p2" -> SimplePort(),
          ),
          edges = Seq()
        ),
        "n2" -> SimpleNode(
          members = Map(
            "n2p1" -> SimplePort(),
          ),
          edges = Seq()
        )
      ), edges = Seq(
        SimpleEdge(source=Seq("p1"), target=Seq("n1", "n1p1")),
        SimpleEdge(source=Seq("n1", "n1p1"), target=Seq("n2", "n2p1")),
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

  it should "create correct ELK labels" in {
    def labelOfShape(node: ElkShape): String = {
      node.getLabels.asScala.head.getText
    }

    val (root, _) = makeGraph()
    val graphNode = root.getChildren.asScala.head

    val p1 = graphNode.getPorts.asScala(0)
    labelOfShape(p1) should equal("p1")

    val n1 = graphNode.getChildren.asScala(0)
    labelOfShape(n1) should equal("n1")
    labelOfShape(n1.getPorts.asScala(0)) should equal("n1p1")
    labelOfShape(n1.getPorts.asScala(1)) should equal("n1p2")

    val n2 = graphNode.getChildren.asScala(1)
    labelOfShape(n2) should equal("n2")
    labelOfShape(n2.getPorts.asScala(0)) should equal("n2p1")
  }
}
