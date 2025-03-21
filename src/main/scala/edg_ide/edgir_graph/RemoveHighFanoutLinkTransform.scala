package edg_ide.edgir_graph

import edgir.elem.elem
import edgir.ref.ref.LibraryPath
import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirEdge

/** Removes links (as nodes - must run before they are collapsed) that are "high-fanout", based on the link type
  * allowlist and parameterized number of sink connections.
  */
class RemoveHighFanoutLinkTransform(minConnects: Int, allowedLinkTypes: Set[LibraryPath]) {

  /** Does the transform, returning the node minus eliminated link nodes, and returning the eliminated links as a map of
    * (containing block, link name) to paths of ports involved in the connection.
    */
  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val allowedLinkNameWraps = node.members
      .collect { // filter by nodes that are links, keep wrapper, extract type
        case (name, EdgirGraph.EdgirNode(linkWrap @ LinkWrapper(linkPath, linkLike), _, _)) =>
          (name, linkWrap, linkLike.`type`)
      }
      .collect {
        case (name, linkWrap, elem.LinkLike.Type.Link(link)) => // extract elaborated link, discard the rest
          (name, linkWrap, link)
      }
      .collect {
        case (name, linkWrap, link) if allowedLinkTypes.contains(link.getSelfClass) => // filter by type
          (name, linkWrap)
      }

    // edges associated with a node, structured as node -> (port name within node, path of other port, edge)
    // this can generate multiple entries per edge, if both ends are nodes
    // TODO it is assumed that the source node is the first component of the path (multicomponent node paths forbidden)
    val allNodeEdges: Map[Seq[String], Seq[(Seq[String], DesignPath, EdgirEdge)]] = node.edges
      .collect { edge =>
        val sourceEdge = edge.source match {
          case Some(sourceNode :: sourceTail) =>
            Seq((Seq(sourceNode), (sourceTail, node.data.path ++ edge.target, edge)))
          case _ => Seq()
        }
        val targetEdge = edge.target match {
          case Some(targetNode :: targetTail) =>
            Seq((Seq(targetNode), (targetTail, node.data.path ++ edge.source, edge)))
          case _ => Seq()
        }
        sourceEdge ++ targetEdge
      }
      .flatten
      .groupMap(_._1)(_._2)

    val highFanoutLinkNameWraps = allowedLinkNameWraps
      .map { case (linkName, linkWrap) =>
        require(linkName.length == 1) // like above, assumed node name is first component of path only
        val connectedCount = allNodeEdges.getOrElse(linkName, Seq()).length
        (linkName, linkWrap, connectedCount)
      }
      .collect {
        case (linkName, linkWrap, connectedCount) if connectedCount >= minConnects => (linkName, linkWrap)
      }
      .toMap

    val filteredEdges = node.edges.map {
      // Transform to tunnels
      case EdgirEdge(data, Seq(sourceNode, _*), target)
        if highFanoutLinkNameWraps.contains(Seq(sourceNode)) =>
        val linkWrap = highFanoutLinkNameWraps(Seq(sourceNode))
        EdgirEdge(EdgeLinkWrapper(linkWrap.path, linkWrap.linkLike), None, target)
      case EdgirEdge(data, source, Seq(targetNode, _*))
        if highFanoutLinkNameWraps.contains(Seq(targetNode)) =>
        val linkWrap = highFanoutLinkNameWraps(Seq(targetNode))
        EdgirEdge(EdgeLinkWrapper(linkWrap.path, linkWrap.linkLike), source, None)
      case edge => edge
    }

    val filteredMembers = node.members
      .filter { case (name, node) => // remove high fanout nodes
        !highFanoutLinkNameWraps.contains(name)
      }
      .map {
        case (name, member: EdgirGraph.EdgirNode) => name -> apply(member)
        case (name, member: EdgirGraph.EdgirPort) => name -> member
      }

    val filteredNode = EdgirGraph.EdgirNode(node.data, filteredMembers, filteredEdges)

    filteredNode
  }
}
