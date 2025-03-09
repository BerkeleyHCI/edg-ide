package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger

class InferEdgeDirectionTransform { // dummy class for logger
}

/** An HGraph transform that infers / fixes up edge directions using the link type and port name.
  *
  * Behavior summary: If the link type has defined sink and source ports: use those For links with bidirectional
  * connections and no sources: the first bidir is a source For exports: direction is inferred from the containing node
  *
  * Algorithm summary: PRECONDITION: block ports are "sources" and link ports are "targets" Build structure of block
  * paths by links and ports For all links, assign sources according to the above summary Traverse edges, updating the
  * direction as needed
  */
object InferEdgeDirectionTransform {
  val logger = Logger.getInstance(classOf[InferEdgeDirectionTransform])

  // For a link and connected ports (my top port -> set of block paths), return the set of
  // block paths that are sources (considered from the block side)
  def sourcePorts(link: LinkWrapper, ports: Map[String, Set[Seq[String]]]): Set[Seq[String]] = {
    // TODO these should be in the IR, perhaps as metadata, instead of hardcoded in the viz code
    val sources = Set(
      "ref", // gnd
      "source",
      "sources",
      "driver",
      "host",
      "master",
      "pull", // SWD, USB, SPI
      "controller" // CAN logic
    )
    val sinks = Set(
      "gnds",
      "sink",
      "sinks",
      "crystal",
      "device",
      "devices", // SWD, USB
      "targets", // I2C
      "peripherals", // SPI
      "transceiver", // CAN logic
      "target_receiver", // I2S
    )
    val bidirs = Set(
      "bidirs",
      "passive",
      "passives",
      "a",
      "b", // UART
      "nodes", // CAN diff
      "pad", // touch
    )
    val allKnownPorts = sources ++ sinks ++ bidirs

    // Sanity check to make sure we aren't missing any ports
    val unknownPorts = ports.collect {
      case (linkPort, blockPorts) if !allKnownPorts.contains(linkPort) =>
        linkPort
    }
    if (unknownPorts.nonEmpty) {
      logger.warn(s"unknown port ${unknownPorts.mkString(", ")} in ${link.path}")
    }

    val strongSourcePorts = ports.collect {
      case (linkPort, blockPorts) if sources.contains(linkPort) =>
        blockPorts
    }.flatten
    if (strongSourcePorts.isEmpty) { // no sources, need to consider bidir ports
      val bidirPorts = ports.collect {
        case (linkPort, blockPorts) if bidirs.contains(linkPort) =>
          blockPorts
      }.flatten
      Set(bidirPorts.headOption).flatten // only take the first bidir port
    } else {
      strongSourcePorts.toSet
    }
  }

  def flipEdge(edge: EdgirGraph.EdgirEdge): EdgirGraph.EdgirEdge = {
    EdgirGraph.EdgirEdge(edge.data, edge.target, edge.source) // invert edge direction
  }

  def apply(node: EdgirGraph.EdgirNode, mySourcePorts: Set[Seq[String]] = Set()): EdgirGraph.EdgirNode = {
    // Aggregate connected block ports by link and link port, as link name -> (link port -> Seq(block path))
    val linkConnectedPorts: Map[String, Map[String, Set[Seq[String]]]] = node.edges
      .flatMap { edge =>
        val edgeTargetTop = edge.target.head
        val targetMember = node.members.get(Seq(edgeTargetTop))
        targetMember match {
          case Some(targetTop: EdgirGraph.EdgirNode) if targetTop.data.isInstanceOf[LinkWrapper] =>
            Some((edge.target.head, (edge.target.tail, edge.source)))
          case _ => None
        }
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2)) // sort by link name, discard the first tuple component in values
      .mapValues { linkPortBlockPathPairs =>
        linkPortBlockPathPairs
          .groupBy(_._1.head)
          .view
          .mapValues(
            _.map(_._2).toSet
          )
          .toMap // same as above, with Set conversion
      }
      .toMap

    val allBlockPorts = linkConnectedPorts.flatMap { case (linkName, linkPortBlockPaths) =>
      linkPortBlockPaths.flatMap(_._2)
    }.toSet

    val blockSourcePorts = linkConnectedPorts.flatMap { case (linkName, linkPortBlockPaths) =>
      // should be safe because this should have been tested above
      val linkWrapper =
        node.members(Seq(linkName)).asInstanceOf[EdgirGraph.EdgirNode].data.asInstanceOf[LinkWrapper]
      sourcePorts(linkWrapper, linkPortBlockPaths)
    }.toSet

    val newEdges = node.edges.map { edge =>
      if (allBlockPorts.contains(edge.source)) { // is a link port
        if (blockSourcePorts.contains(edge.source)) {
          edge // current order is correct, block is source
        } else {
          flipEdge(edge) // invert edge direction
        }
      } else { // is (probably?) a hierarchy port
        if (!node.members.contains(edge.target)) {
          logger.warn(s"non-block and non-boundary port referenced by edge ${edge.target}")
        }
        if (mySourcePorts.contains(edge.target)) { // correct is (port is target from inside)
          edge
        } else {
          flipEdge(edge)
        }
      }
    }
    val newMembers = node.members.map { // recurse into child nodes
      case (name, member: EdgirGraph.EdgirNode) =>
        val memberSourcePorts = blockSourcePorts.collect {
          case path if path.startsWith(name) =>
            path.drop(name.length)
        }
        name -> apply(member, memberSourcePorts)
      case (name, member: EdgirGraph.EdgirPort) => name -> member
    }
    EdgirGraph.EdgirNode(node.data, newMembers, newEdges)
  }
}
