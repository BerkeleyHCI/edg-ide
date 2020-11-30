package edg_ide

// Hierarchy graph data type

// TODO this really should be a union type instead of a trait, but because of limitations of Scala
// this is where we are
sealed trait HGraphNodeMember[NodeType, PortType, EdgeType] {
}

// TODO support undirected edges?
case class HGraphEdge[EdgeType](
  data: EdgeType,
  source: Seq[String],
  target: Seq[String],
) {
  def ports: Seq[String] = source ++ target
}

case class HGraphPort[PortType](
  data: PortType,
)

case class HGraphNode[NodeType, PortType, EdgeType](
  data: NodeType,
  members: Map[String, HGraphNodeMember[NodeType, PortType, EdgeType]],
  edges: Seq[HGraphEdge[EdgeType]],
) extends HGraphNodeMember[NodeType, PortType, EdgeType]
