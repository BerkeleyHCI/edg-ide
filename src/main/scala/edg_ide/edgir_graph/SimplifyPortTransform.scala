package edg_ide.edgir_graph


/**
  * An HGraph transform that eliminates sub-ports from edge connects,
  * since those aren't supported by ELK
  */
object SimplifyPortTransform {
  def simplify(path: Seq[String], context: EdgirGraph.EdgirNode): Seq[String] = {
    path match {
      case Seq() => Seq()
      case Seq(head, tail@_*) => context.members(head) match {
        case subnode: EdgirGraph.EdgirNode => Seq(head) ++ simplify(tail, subnode)
        case _: EdgirGraph.EdgirPort => Seq(head)
      }
    }
  }

  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val newEdges = node.edges.map { edge =>
      val sourceSimplified = simplify(edge.source, node)
      val targetSimplified = simplify(edge.target, node)
      EdgirGraph.EdgirEdge(edge.data, sourceSimplified, targetSimplified)
    }
    val newMembers = node.members.mapValues {
      case member: EdgirGraph.EdgirNode => apply(member)
      case member: EdgirGraph.EdgirPort => member
    }.toMap
    EdgirGraph.EdgirNode(node.data, newMembers, newEdges)
  }
}
