package edg_ide.edgir_graph

import edg_ide.edgir_graph.EdgirGraph.EdgirEdge


/** Prunes (removes) ports that are named like array ports that are disconnected, except for the next free one.
  *
  * TODO remove once we have proper (dynamic) array ports, rather than the pre-unrolled array ports
  */
object PruneArrayPortsTransform {
  // Returns the integer index and prefix if the input is of the form something[index]
  protected def arrayNameToIndexOption(name: String): Option[(String, Int)] = {
    val leftIndex = name.lastIndexOf("[")
    val rightIndex = name.lastIndexOf("]")
    if (leftIndex == -1 || rightIndex == -1 || leftIndex >= rightIndex) {
      None
    } else {
      val contained = name.slice(leftIndex + 1, rightIndex)
      val prefix = name.slice(0, leftIndex)
      (contained.toIntOption).map((prefix, _))
    }
  }

  // Returns the next free non-negative integer not in the prior set
  protected def nextArrayIndex(prior: Set[Int]): Int = {
    var i = 0
    while (prior.contains(i)) {
      i += 1
    }
    i
  }

  def apply(node: EdgirGraph.EdgirNode, connectedPorts: Set[String] = Set()): EdgirGraph.EdgirNode = {
    val connectedInnerPortsByBlock = node.edges.flatMap { case EdgirEdge(data, source, target) =>
      val sourceOption = source match {
        case Seq(block, port) => Some((block, port))
        case _ => None
      }
      val targetOption = target match {
        case Seq(block, port) => Some((block, port))
        case _ => None
      }
      Seq(sourceOption, targetOption).flatten
    }.groupBy(_._1).view.mapValues(_.map(_._2).toSet)

    val nextFreeArrayPort = connectedPorts.flatMap { arrayNameToIndexOption }  // to (prefix, index) pairs
        .groupBy(_._1)
        .map { case (name, pairs) =>
          val indices = pairs.map(_._2)
          name -> nextArrayIndex(indices)
        }

    val newMembers = node.members.map {  // recurse into child nodes
      case (name, member: EdgirGraph.EdgirNode) =>
        name -> apply(member, connectedInnerPortsByBlock.getOrElse(name, Set()))
      case (name, member: EdgirGraph.EdgirPort) =>
        name -> member
    } .filter {
      case (name, member: EdgirGraph.EdgirPort) =>
        arrayNameToIndexOption(name) match {
          case Some((prefix, index)) =>
            val nextFreeIndex = nextFreeArrayPort.getOrElse(prefix, 0)
            connectedPorts.contains(name) || index == nextFreeIndex
          case None => true
        }

      case _ => true  // pass through nodes
    }

    EdgirGraph.EdgirNode(node.data, newMembers, node.edges)
  }
}
