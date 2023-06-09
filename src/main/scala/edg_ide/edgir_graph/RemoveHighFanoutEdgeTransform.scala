package edg_ide.edgir_graph

import edgir.elem.elem
import edgir.ref.ref.LibraryPath
import edg.wir.DesignPath
import edg_ide.edgir_graph.EdgirGraph.EdgirEdge

/** Removes links (as edges - must run AFTER collapse - prevents weird interactions with bridge removal) that
  * are "high-fanout", based on the link type allowlist and parameterized number of sink connections.
  */
class RemoveHighFanoutEdgeTransform(minConnects: Int, allowedLinkTypes: Set[LibraryPath]) {
  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val highFanoutLinks = node.edges
      .collect { case EdgirEdge(EdgeLinkWrapper(linkPath, linkLike), source, target) =>
        (
          linkPath,
          linkLike.`type`,
          linkLike,
          source,
          target
        ) // extract link path, linkLike, source, and targets
      }
      .collect { case (linkPath, elem.LinkLike.Type.Link(link), linkLike, source, target) =>
        (linkPath, link, linkLike, source, target) // extract elaborated link
      }
      .collect {
        case (linkPath, link, linkLike, source, target) if allowedLinkTypes.contains(link.getSelfClass) =>
          (linkPath, linkLike, source, target) // filter by type
      }
      .groupBy(_._1)
      .map { case (linkPath, pairs) =>
        val linkLike = pairs.head._2 // assume this is the same?
        val allSources = pairs.map(_._3)
        val allTargets = pairs.map(_._4)
        linkPath -> (linkLike, (allSources ++ allTargets).toSet)
      }
      .filter { case (linkPath, (linkLike, allTargets)) =>
        allTargets.size >= minConnects // filter by high fanout
      }

    val filteredEdges = node.edges.filter { // remove high fanout link edges
      case EdgirEdge(EdgeLinkWrapper(linkPath, linkLike), source, target) =>
        !highFanoutLinks.isDefinedAt(linkPath)
      case edge => true
    } ++ highFanoutLinks.flatMap { case (linkPath, (linkLike, allTargets)) =>
      allTargets.map { case target =>
        EdgirEdge(EdgeLinkWrapper(linkPath, linkLike), target, target)
      }
    }

    val filteredMembers = node.members.map { // recursively apply to subnodes
      case (name, member: EdgirGraph.EdgirNode) => name -> apply(member)
      case (name, member: EdgirGraph.EdgirPort) => name -> member
    }

    val filteredNode = EdgirGraph.EdgirNode(node.data, filteredMembers, filteredEdges)

    filteredNode
  }
}
