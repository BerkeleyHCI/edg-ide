package edg_ide.edgir_graph

import scala.collection.SeqMap

/** Given a map of (group names -> node names in that group), creates a node for each group, then moves prior nodes in a
  * group into that node. Edges between nodes of the same group are moved into the group node, while edges between nodes
  * are transformed into tunnels (implemented as degenerate edges)
  */
object GroupingTransform {
  def apply(container: EdgirGraph.EdgirNode, groups: SeqMap[String, Seq[String]]): EdgirGraph.EdgirNode = {
    val nodeToGroup = groups.flatMap { case (groupName, members) =>
      members.map(Seq(_) -> groupName) // convert to pathname
    }
    val groupedNodes = container.members.toSeq.map { case (pathName, node) => // toSeq conversion preserves order
      nodeToGroup.getOrElse(pathName, None) -> (pathName -> node)
    }.groupBy(_._1).view.mapValues(_.map(_._2).to(SeqMap))

    val groupedEdges = container.edges.flatMap { edge =>
      val srcGroup = edge.source.map(source => nodeToGroup.getOrElse(Seq(source.head), None))
      val dstGroup = edge.target.map(target => nodeToGroup.getOrElse(Seq(target.head), None))
      if (srcGroup == dstGroup) { // if in same group, move to group
        Seq(srcGroup.getOrElse(dstGroup) -> edge)
      } else { // else create tunnel
        val sourceOpt = edge.source match {
          case Some(source) => Seq(srcGroup.getOrElse(None) -> EdgirGraph.EdgirEdge(
              data = edge.data,
              source = Some(source),
              target = None
            ))
          case _ => Seq()
        }
        val targetOpt = edge.target match {
          case Some(target) => Seq(dstGroup.getOrElse(None) -> EdgirGraph.EdgirEdge(
              data = edge.data,
              source = None,
              target = Some(target)
            ))
          case _ => Seq()
        }
        sourceOpt ++ targetOpt
      }
    }.groupBy(_._1).view.mapValues(_.map(_._2))

    val groupsMembers = groups.keys.map { groupName =>
      Seq(groupName) -> EdgirGraph.EdgirNode(
        data = GroupWrapper(container.data.path, groupName),
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
