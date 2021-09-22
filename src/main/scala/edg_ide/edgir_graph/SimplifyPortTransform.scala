package edg_ide.edgir_graph

import com.intellij.openapi.diagnostic.Logger

import scala.collection.SeqMap


/**
  * An HGraph transform that eliminates sub-ports from edge connects,
  * since those aren't supported by ELK
  */
object SimplifyPortTransform {
  val logger = Logger.getInstance(classOf[InferEdgeDirectionTransform])

  def simplify(path: Seq[String], context: EdgirGraph.EdgirNode): Option[Seq[String]] = {
    path match {
      case Seq() => Some(Seq())
      case Seq(head, tail@_*) => context.members.get(head) match {
        case Some(subnode: EdgirGraph.EdgirNode) =>
          simplify(tail, subnode) match {
            case Some(recursiveSuffix) => Some(Seq(head) ++ recursiveSuffix)
            case None => None
          }
        case Some(_: EdgirGraph.EdgirPort) => Some(Seq(head))
        case None => None
      }
    }
  }

  def apply(node: EdgirGraph.EdgirNode): EdgirGraph.EdgirNode = {
    val newEdges = node.edges.flatMap { edge =>
      val sourceSimplified = simplify(edge.source, node)
      val targetSimplified = simplify(edge.target, node)
      (sourceSimplified, targetSimplified) match {
        case (Some(sourceSimplified), Some(targetSimplified)) => Some(
          EdgirGraph.EdgirEdge(edge.data, sourceSimplified, targetSimplified)
        )
        case _ =>
          logger.warn(s"unknown source ${edge.source} or target ${edge.target}")
          None
      }
    }
    val newMembers = node.members.to(SeqMap).view.mapValues {
      case member: EdgirGraph.EdgirNode => apply(member)
      case member: EdgirGraph.EdgirPort => member
    }.to(SeqMap)
    EdgirGraph.EdgirNode(node.data, newMembers, newEdges)
  }
}
