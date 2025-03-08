package edg_ide.edgir_graph

import scala.collection.SeqMap

/**
  * Given a map of (group names -> node names in that group), creates a node for each group,
  * then moves prior nodes in a group into that node.
  * Edges between nodes of the same group are moved into the group node, while edges between nodes
  * are transformed into tunnels (implemented as degenerate edges)
  */
class GroupingTransform {
  def apply(container: EdgirGraph.EdgirNode, groups: SeqMap[String, Seq[String]]) = {
    val nodeToGroup = groups.flatMap { case (groupName, members) =>
      members.map(Seq(_) -> groupName)  // convert to pathname
    }
    val groupedNodes = container.members.map { case (pathName, node) =>
      nodeToGroup.getOrElse(pathName, None) -> node
    }.groupBy(_._1)
    val groupedEdges = container.edges.flatMap { edge =>
      val srcGroup = nodeToGroup.getOrElse(edge.source, None)
      val dstGroup = nodeToGroup.getOrElse(edge.target, None)
      if (srcGroup == dstGroup) { // if in same group, move to group
        Seq(srcGroup -> edge)
      } else { // else create degenerate edge / tunnel
        Seq(None -> edge)
      }
    }.groupBy(_._1)

//    val newContainerMembers =
  }
}
