package edg_ide.edgir_graph

/** Simple transform that collapses all adapter and bridge (pseudo)blocks
  */
object CollapseBridgeTransform extends CollapseNodeTransform {
  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val linkNames = node.members.collect {
      case (name, _) if name.head.startsWith("(bridge)") || name.head.startsWith("(adapter)") =>
        name
    }

    val newNode = linkNames.foldLeft(node) { case (prevNode, linkName) =>
      collapse(prevNode, linkName, edgeDatas => edgeDatas.head) // TODO better edge data
    }

    val newNodeNewMembers = newNode.members.map { // recurse into child nodes
      case (name, member: EdgirGraph.EdgirNode) => name -> apply(member)
      case (name, member: EdgirGraph.EdgirPort) => name -> member
    }

    EdgirGraph.EdgirNode(newNode.data, newNodeNewMembers, newNode.edges)
  }
}
