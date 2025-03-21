package edg_ide.edgir_graph.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.eclipse.elk.graph._
import edg_ide.edgir_graph.{HGraphEdge, HGraphNode, HGraphNodeMember, HGraphPort, HierarchyGraphElk}

import scala.collection.SeqMap
import scala.jdk.CollectionConverters.ListHasAsScala

class HierarchyGraphElkTest extends AnyFlatSpec with Matchers {
  sealed trait SimpleNodeMember extends HGraphNodeMember[String, String, String] {}

  case class SimpleNode(
      override val data: String,
      override val members: SeqMap[Seq[String], SimpleNodeMember],
      override val edges: Seq[HGraphEdge[String]],
  ) extends HGraphNode[String, String, String] with SimpleNodeMember {}

  case class SimplePort(
      override val data: String,
  ) extends HGraphPort[String] with SimpleNodeMember {}

  case class SimpleEdge(
      override val data: String,
      override val source: Option[Seq[String]],
      override val target: Option[Seq[String]]
  ) extends HGraphEdge[String] {}

  behavior.of("HGraphNodeToElkNode")

  def makeGraph(): (ElkNode, SeqMap[Seq[String], ElkConnectableShape]) = {
    val simpleGraph = SimpleNode(
      "root",
      members = SeqMap(
        Seq("p1") -> SimplePort("p1data"),
        Seq("n1") -> SimpleNode(
          "n1data",
          members = SeqMap(
            Seq("n1p1") -> SimplePort("n1p1data"),
            Seq("n1p2") -> SimplePort("n1p2data"),
          ),
          edges = Seq()
        ),
        Seq("n2") -> SimpleNode(
          "n2data",
          members = SeqMap(
            Seq("n2p1") -> SimplePort("n2p1data"),
          ),
          edges = Seq()
        )
      ),
      edges = Seq(
        SimpleEdge("edge1", source = Some(Seq("p1")), target = Some(Seq("n1", "n1p1"))),
        SimpleEdge("edge2", source = Some(Seq("n1", "n1p1")), target = Some(Seq("n2", "n2p1"))),
      )
    )
    val (root, elkElementsMap) = HierarchyGraphElk.HGraphNodeToElkNode(simpleGraph, "", None)

    (root, elkElementsMap)
  }

  it should "return a map of internal elements" in {
    val (_, elkElementsMap) = makeGraph()

    (elkElementsMap should contain).key(Seq("p1"))
    elkElementsMap(Seq("p1")) shouldBe a[ElkPort]
  }

  it should "create ELK elements" in {
    val (root, _) = makeGraph()

    root.getChildren should have size 2
    root.getPorts should have size 1
    root.getContainedEdges should have size 2
  }

  it should "create correct ELK labels" in {
    def labelsOfShape(node: ElkShape): Seq[String] = {
      node.getLabels.asScala.map(_.getText).toSeq
    }

    val (root, _) = makeGraph()

    val p1 = root.getPorts.get(0)
    labelsOfShape(p1) should equal(Seq("p1"))

    val n1 = root.getChildren.get(0)
    labelsOfShape(n1) should equal(Seq("n1", "n1data"))
    labelsOfShape(n1.getPorts.get(0)) should equal(Seq("n1p1"))
    labelsOfShape(n1.getPorts.get(1)) should equal(Seq("n1p2"))

    val n2 = root.getChildren.get(1)
    labelsOfShape(n2) should equal(Seq("n2", "n2data"))
    labelsOfShape(n2.getPorts.get(0)) should equal(Seq("n2p1"))
  }
}
