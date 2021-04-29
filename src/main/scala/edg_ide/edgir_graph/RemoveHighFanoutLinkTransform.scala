package edg_ide.edgir_graph

import edg.elem.elem
import edg.ref.ref.LibraryPath
import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirEdge

/** Removes links (as nodes - must run before they are collapsed) that are "high-fanout",
  * based on the link type allowlist and parameterized number of sink connections.
  */
class RemoveHighFanoutLinkTransform(minConnects: Int, allowedLinkTypes: Set[LibraryPath]) {
  /** Does the transform, returning the node minus eliminated link nodes, and returning the eliminated links
    * as a map of (containing block, link name) to paths of ports involved in the connection.
    */
  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val allowedLinkNameWraps = node.members.collect {  // filter by nodes that are links, keep wrapper, extract type
      case (name, EdgirGraph.EdgirNode(linkWrap @ LinkWrapper(linkPath, linkLike), _, _)) =>
        (name, linkWrap, linkLike.`type`)
    } .collect { case (name, linkWrap, elem.LinkLike.Type.Link(link)) =>  // extract elaborated link, discard the rest
      (name, linkWrap, link)
    } .collect { case (name, linkWrap, link) if allowedLinkTypes.contains(link.getSelfClass) =>  // filter by type
      (name, linkWrap)
    }

    // edges associated with a node, structured as node -> (port name within node, path of other port, edge)
    val allNodeEdges: Map[String, Seq[(String, DesignPath, EdgirEdge)]] = node.edges.collect {
      case edge @ EdgirEdge(data, source, target) =>  // generate all pairs (node name, port within node, edge)
        (source, target) match {
          case (Seq(sourceNode, sourcePort), Seq(targetNode, targetPort)) => // both interior connects, 2 pairs to create
            Seq((sourceNode, sourcePort, node.data.path + targetNode + targetPort, edge),
              (targetNode, targetPort, node.data.path + sourceNode + sourcePort, edge)
            )
          case (Seq(sourceNode, sourcePort), Seq(targetPort)) => // boundary port
            Seq((sourceNode, sourcePort, node.data.path + targetPort, edge))
          case (Seq(sourcePort), Seq(targetNode, targetPort)) => // boundary port
            Seq((targetNode, targetPort, node.data.path + sourcePort, edge))
          case (Seq(sourcePort), Seq(targetPort)) => // neither connects to internal node, none to genererate
            Seq()
        }
    }.flatten
        .groupBy(_._1)
        .mapValues {
          _.map { case (nodeName, portName, otherPath, edge) =>  // discard nodeName from values
            (portName, otherPath, edge)
          }
        }.toMap

    val highFanoutLinkNameWraps = allowedLinkNameWraps.map { case (linkName, linkWrap) =>
      val connectedCount = allNodeEdges.getOrElse(linkName, Seq()).length
      (linkName, linkWrap, connectedCount)
    } .collect {
      case (linkName, linkWrap, connectedCount) if connectedCount >= minConnects => (linkName, linkWrap)
    }.toMap

    val filteredEdges = node.edges.map {
          // Transform to degenerate edges
      case EdgirEdge(data, Seq(sourceNode, sourcePort), target) if highFanoutLinkNameWraps.contains(sourceNode) =>
        val linkWrap = highFanoutLinkNameWraps(sourceNode)
        EdgirEdge(EdgeLinkWrapper(linkWrap.path, linkWrap.linkLike), target, target)
      case EdgirEdge(data, source, Seq(targetNode, targetPort)) if highFanoutLinkNameWraps.contains(targetNode) =>
        val linkWrap = highFanoutLinkNameWraps(targetNode)
        EdgirEdge(EdgeLinkWrapper(linkWrap.path, linkWrap.linkLike), source, source)
      case edge => edge
    }

    val filteredMembers = node.members.filter { case (name, node) =>  // remove high fanout nodes
      !highFanoutLinkNameWraps.contains(name)
    }.map {
      case (name, member: EdgirGraph.EdgirNode) => name -> apply(member)
      case (name, member: EdgirGraph.EdgirPort) => name -> (member)
    }

    val filteredNode = EdgirGraph.EdgirNode(node.data, filteredMembers, filteredEdges)

    filteredNode
  }
}
