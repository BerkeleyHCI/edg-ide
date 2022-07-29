package edg_ide.edgir_graph


/**
  * Simple transform that collapses all links
  */
object CollapseLinkTransform extends CollapseNodeTransform {
  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val linkNameDatas = node.members.collect {
      case (name, member: EdgirGraph.EdgirNode) if member.data.isInstanceOf[LinkWrapper] =>
        (name, member.data.asInstanceOf[LinkWrapper])
    }

    val newNode = linkNameDatas.foldLeft(node) { case (prevNode, (linkName, linkData)) =>
      val edgeData = EdgeLinkWrapper(linkData.path, linkData.linkLike)
      collapse(prevNode, linkName, _ => edgeData)
    }

    val newNodeNewMembers = newNode.members.map {  // recurse into child nodes
      case (name, member: EdgirGraph.EdgirNode) => name -> apply(member)
      case (name, member: EdgirGraph.EdgirPort) => name -> member
    }

    EdgirGraph.EdgirNode(newNode.data, newNodeNewMembers, newNode.edges)
  }
}
