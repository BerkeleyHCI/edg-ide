package edg_ide.edgir_graph

import scala.collection.SeqMap

/** Given a map of (group names -> node names in that group), creates a node for each group, then moves prior nodes in a
  * group into that node. Edges between nodes of the same group are moved into the group node, while edges between nodes
  * are transformed into tunnels (implemented as degenerate edges)
  */
class GroupingTransform {
  def apply(container: EdgirGraph.EdgirNode, groups: SeqMap[String, Seq[String]]): EdgirGraph.EdgirNode = {
    val nodeToGroup = groups.flatMap { case (groupName, members) =>
      members.map(Seq(_) -> groupName) // convert to pathname
    }
    val groupedNodes = container.members.toSeq.map { case (pathName, node) => // toSeq conversion preserves order
      nodeToGroup.getOrElse(pathName, None) -> (pathName -> node)
    }.groupBy(_._1).view.mapValues(_.map(_._2).to(SeqMap))

    val groupedEdges = container.edges.flatMap { edge =>
      val srcGroup = nodeToGroup.getOrElse(Seq(edge.source.head), None)
      val dstGroup = nodeToGroup.getOrElse(Seq(edge.target.head), None)
      if (srcGroup == dstGroup) { // if in same group, move to group
        Seq(srcGroup -> edge)
      } else { // else create degenerate edge / tunnel
        Seq(
          srcGroup -> EdgirGraph.EdgirEdge(data = edge.data, source = edge.source, target = edge.source),
          dstGroup -> EdgirGraph.EdgirEdge(data = edge.data, source = edge.target, target = edge.target),
        )
      }
    }.groupBy(_._1).view.mapValues(_.map(_._2))

    val groupsMembers = groups.keys.map { groupName =>
      Seq(groupName) -> EdgirGraph.EdgirNode(
        data = container.data,
        members = groupedNodes.getOrElse(groupName, SeqMap()),
        edges = groupedEdges.getOrElse(groupName, Seq())
      )
    }

    EdgirGraph.EdgirNode(
      data = container.data,
      members = groupedNodes.getOrElse(None, SeqMap()) ++ groupsMembers,
      edges = groupedEdges.getOrElse(None, Seq())
    )
  }
}
