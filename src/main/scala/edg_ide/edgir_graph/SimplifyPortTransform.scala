package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger

import scala.collection.SeqMap

class SimplifyPortTransform { // dummy class for logger
}

/** An HGraph transform that finds edge port references that don't exist as ports, and drops path components
  * until they are resolvable. Logs an error if it's dropped all the way to an empty path, in which case the
  * edge is discarded.
  */
object SimplifyPortTransform {
  val logger = Logger.getInstance(classOf[SimplifyPortTransform])

  def simplify(
      portPath: Seq[String],
      portPathTail: Seq[String],
      parent: EdgirGraph.EdgirNode
  ): Option[Seq[String]] = {
    portPath match {
      case Seq() => None
      case portPath =>
        parent.members.get(portPath) match {
          case Some(_: EdgirGraph.EdgirPort)    => Some(portPath)
          case Some(node: EdgirGraph.EdgirNode) => simplify(portPathTail, Seq(), node).map(portPath ++ _)
          case _ => simplify(portPath.init, portPath.last +: portPathTail, parent)
        }
    }
  }

  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val newEdges = node.edges.flatMap { edge =>
      val sourceSimplified = simplify(edge.source, Seq(), node)
      val targetSimplified = simplify(edge.target, Seq(), node)
      (sourceSimplified, targetSimplified) match {
        case (Some(sourceSimplified), Some(targetSimplified)) =>
          Some(
            EdgirGraph.EdgirEdge(edge.data, sourceSimplified, targetSimplified)
          )
        case (None, None) =>
          logger.warn(s"unknown source ${edge.source} and target ${edge.target}, discarding edge")
          None
        case (Some(_), None) =>
          logger.warn(s"unknown target ${edge.target}, discarding edge")
          None
        case (None, Some(_)) =>
          logger.warn(s"unknown source ${edge.source}, discarding edge")
          None
      }
    }
    val newMembers = node.members
      .to(SeqMap)
      .view
      .mapValues {
        case member: EdgirGraph.EdgirNode => apply(member)
        case member: EdgirGraph.EdgirPort => member
      }
      .to(SeqMap)
    EdgirGraph.EdgirNode(node.data, newMembers, newEdges)
  }
}
