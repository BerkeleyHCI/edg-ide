package edg_ide.edgir_graph

import edg.elem.elem
import edg.ElemBuilder.LibraryPath
import edg.ref.ref.LibraryPath
import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirEdge

import scala.collection.SeqMap

/** Removes links (as nodes - must run before they are collapsed) that are "high-fanout",
  * based on the link type allowlist and parameterized number of sink connections.
  */
class RemoveHighFanoutLinkTransform(minConnects: Int, allowedLinkTypes: Set[LibraryPath]) {
//  private val minConnects = 4  // TODO sink only?
//  private val allowedLinkTypes = ,
//  )

  /** Does the transform, returning the node minus eliminated link nodes, and returning the eliminated links
    * as a map of (containing block, link name) to paths of ports involved in the connection.
    */
  def apply(node: EdgirGraph.EdgirNode): (EdgirGraph.EdgirNode, Map[(DesignPath, String), Seq[DesignPath]]) = {
    val allowedLinkNodes = node.members.collect {  // filter by nodes that are links, extract type
      case (name, EdgirGraph.EdgirNode(LinkWrapper(linkPath, linkLike), _, _)) =>
        (name, linkLike.`type`)
    } .collect { case (name, elem.LinkLike.Type.Link(link)) =>  // extract elaborated link, discard the rest
      (name, link)
    } .collect { case (name, link) if link.superclasses.toSet.subsetOf(allowedLinkTypes) =>  // filter by type
      name
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

    val highFanoutLinkNames = allowedLinkNodes.map { linkName =>
      val connectedCount = allNodeEdges.getOrElse(linkName, Seq()).length
      (linkName, connectedCount)
    } .collect {
      case (linkName, connectedCount) if connectedCount >= minConnects => linkName
    }.toSet

    val filteredMembers = node.members.filter { case (name, node) =>  // remove high fanout nodes
      !highFanoutLinkNames.contains(name)
    }
    val filteredEdges = node.edges.filter {  // remove edges connecting to that node
      case EdgirEdge(data, Seq(sourceNode, sourcePort), target) if highFanoutLinkNames.contains(sourceNode) => false
      case EdgirEdge(data, source, Seq(targetNode, targetPort)) if highFanoutLinkNames.contains(targetNode) => false
      case _ => true
    }

    val eliminatedLinks = highFanoutLinkNames.map { linkName =>
      val connectedPaths = allNodeEdges.getOrElse(linkName, Seq()).map { case (portName, otherPath, edge) =>
        otherPath
      }
      (node.data.path, linkName) -> connectedPaths
    }.toMap

    val recursiveResults = filteredMembers.map {
      case (name, member: EdgirGraph.EdgirNode) => name -> apply(member)
      case (name, member: EdgirGraph.EdgirPort) => name -> (member, Map())
    }
    val recursiveMembers = recursiveResults.mapValues(_._1).to(SeqMap)
    val recursiveEliminated = recursiveResults.values.map(_._2).flatten

    val filteredNode = EdgirGraph.EdgirNode(node.data, recursiveMembers, filteredEdges)

    (filteredNode, eliminatedLinks ++ recursiveEliminated)
  }
}
