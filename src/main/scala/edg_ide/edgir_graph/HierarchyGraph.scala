package edg_ide.edgir_graph

import scala.collection.SeqMap

// Hierarchy graph data type

// TODO this really should be a union type instead of a trait, but because of limitations of Scala
// this is where we are
trait HGraphNodeMember[+NodeType, +PortType, +EdgeType] {}

// TODO support undirected edges?
trait HGraphEdge[EdgeType] {
  val data: EdgeType
  val source: Seq[String]
  val target: Seq[String]

  def ports: Seq[String] = source ++ target
}

trait HGraphPort[PortType] extends HGraphNodeMember[Nothing, PortType, Nothing] {
  val data: PortType
}

trait HGraphNode[NodeType, PortType, EdgeType] extends HGraphNodeMember[NodeType, PortType, EdgeType] {
  val data: NodeType
  val members: SeqMap[Seq[String], HGraphNodeMember[NodeType, PortType, EdgeType]]
  val edges: Seq[HGraphEdge[EdgeType]]
}
