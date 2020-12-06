package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger


class InferEdgeDirectionTransform {  // dummy class for logger
}

/**
  * An HGraph transform that infers / fixes up edge directions using the link type and port name.
  *
  * Behavior summary:
  * If the link type has defined sink and source ports: use those
  * For links with bidirectional connections and no sources: the first bidir is a source
  * For exports: direction is inferred from the containing node
  *
  * Algorithm summary:
  * PRECONDITION: block ports are "sources" and link ports are "targets"
  * Build structure of block paths by links and ports
  * For all links, assign sources according to the above summary
  * Traverse edges, updating the direction as needed
  */
object InferEdgeDirectionTransform {
  val logger = Logger.getInstance(classOf[InferEdgeDirectionTransform])

  // For a link and connected ports (my top port -> set of block paths), return the set of
  // block paths that are sources (considered from the block side)
  def sourcePorts(link: LinkWrapper, ports: Map[String, Seq[Seq[String]]]): Set[Seq[String]] = {
    // TODO these should be in the IR, perhaps as metadata, instead of hardcoded in the viz code
    val sources = Set(
      "source", "single_sources",
      "driver",
      "host", "master", "pull",  // SWD, USB, SPI
      "controller",  // CAN logic
    )
    val sinks = Set(
      "sinks",
      "crystal",
      "device",  // SWD, USB, SPI
      "transceiver",  // CAN logic
    )
    val bidirs = Set(
      "bidirs", "passives",
      "a", "b",  // UART
      "node",  // CAN diff
    )
    val allKnownPorts = sources ++ sinks ++ bidirs

    // Sanity check to make sure we aren't missing any ports
    val unknownPorts = ports.collect { case (linkPort, blockPorts) if !allKnownPorts.contains(linkPort) =>
      linkPort
    }
    if (unknownPorts.nonEmpty) {
      logger.warn(s"unknown port ${unknownPorts.mkString(", ")}")
    }

    val strongSourcePorts = ports.collect { case (linkPort, blockPorts) if sources.contains(linkPort) =>
      blockPorts
    }.flatten
    if (strongSourcePorts.isEmpty) {  // no sources, need to consider bidir ports
      val bidirPorts = ports.collect { case (linkPort, blockPorts) if bidirs.contains(linkPort) =>
        blockPorts
      }.flatten
      Set(bidirPorts.headOption.getOrElse(Seq()))  // only take the first bidir port
    } else {
      strongSourcePorts.toSet
    }
  }

  def apply(node: EdgirGraph.EdgirNode, mySourcePorts: Set[String] = Set()): EdgirGraph.EdgirNode = {
    // Aggregate connected block ports by link and link port
    val linkConnectedPorts: Map[String, Map[String, Seq[Seq[String]]]] = node.edges.flatMap { edge =>
      val edgeTargetTop = edge.target.head
      val targetMember = node.members(edgeTargetTop)
      targetMember match {
        case targetTop: EdgirGraph.EdgirNode if targetTop.data.isInstanceOf[LinkWrapper] =>
          Some((edge.target(0), (edge.target(1), edge.source)))
        case _ => None
      }
    }   .groupBy(_._1).mapValues(_.map(_._2))  // sort by link name, discard the first tuple component in values
        .mapValues { linkPortBlockPathPairs =>
          linkPortBlockPathPairs.groupBy(_._1).mapValues(_.map(_._2))  // same as above, with Set conversion
    }

    val allBlockPorts = linkConnectedPorts.flatMap { case (linkName, linkPortBlockPaths) =>
      linkPortBlockPaths.flatMap(_._2)
    }.toSet

    val blockSourcePorts = linkConnectedPorts.flatMap { case (linkName, linkPortBlockPaths) =>
      // should be safe because this should have been tested above
      val linkWrapper = node.members(linkName).asInstanceOf[EdgirGraph.EdgirNode].data.asInstanceOf[LinkWrapper]
      sourcePorts(linkWrapper, linkPortBlockPaths)
    }.toSet

    val newEdges = node.edges.map { edge =>
      if (allBlockPorts.contains(edge.source)) {  // is a link port
        if (blockSourcePorts.contains(edge.source)) {
          edge  // current order is correct, block is source
        } else {
          EdgirGraph.EdgirEdge(edge.data, edge.target, edge.source)  // invert edge direction
        }
      } else {  // is (probably?) a hierarchy port
        require(edge.target.length == 1)
        val edgeBlockPort = edge.target.head
        if (mySourcePorts.contains(edgeBlockPort)) {  // correct is (port is target from inside)
          edge
        } else {
          EdgirGraph.EdgirEdge(edge.data, edge.target, edge.source)
        }
      }
    }
    val newMembers = node.members.map {  // recurse into child nodes
      case (name, member: EdgirGraph.EdgirNode) =>
        val memberSourcePorts = blockSourcePorts.collect { case path if path.head == name =>
          require(path.tail.length == 1)
          path.tail.head
        }
        name -> apply(member, memberSourcePorts)
      case (name, member: EdgirGraph.EdgirPort) => name -> member
    }
    EdgirGraph.EdgirNode(node.data, newMembers, newEdges)
  }
}
