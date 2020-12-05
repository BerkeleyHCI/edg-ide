package edg_ide

import scala.collection.JavaConverters._
import edg.elem.elem
import edg.expr.expr
import edg.ref.ref


// Should be an union type, but not supported in Scala, so here's wrappers =(
sealed trait NodeDataWrapper {
}
case class BlockWrapper(blockLike: elem.BlockLike) extends NodeDataWrapper
case class LinkWrapper(linkLike: elem.LinkLike) extends NodeDataWrapper


object EdgirGraph {
  // Edgir graph is a type parameterization of the HGraphNode
//  type EdgirNodeMember = HGraphNodeMember[NodeDataWrapper, elem.PortLike, String]
//  type EdgirNode = HGraphNode[NodeDataWrapper, elem.PortLike, String]
//  val EdgirNode = HGraphNode[NodeDataWrapper, elem.PortLike, String]
//  type EdgirPort = HGraphPort[elem.PortLike]
//  val EdgirPort = HGraphPort
//  type EdgirEdge = HGraphEdge[String]
//  val EdgirEdge = HGraphEdge

  sealed trait EdgirNodeMember extends HGraphNodeMember[NodeDataWrapper, elem.PortLike, String] {
  }

  case class EdgirNode(
    override val data: NodeDataWrapper,
    override val members: Map[String, EdgirNodeMember],
    override val edges: Seq[HGraphEdge[String]]
  ) extends HGraphNode[NodeDataWrapper, elem.PortLike, String] with EdgirNodeMember {}

  case class EdgirPort(
    override val data: elem.PortLike
  ) extends HGraphPort[elem.PortLike] with EdgirNodeMember {}

  case class EdgirEdge(
    override val data: String,
    override val source: Seq[String],
    override val target: Seq[String]
  ) extends HGraphEdge[String]

  /**
    * Simple wrapper around blockLikeToNode that provides the blockLike wrapper around the block
    */
  def blockToNode(block: elem.HierarchyBlock, lib: EdgirLibrary): EdgirNode = {
    blockLikeToNode(
      elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(block)),
      lib)
  }

  def blockLikeToNode(blockLike: elem.BlockLike, lib: EdgirLibrary): EdgirNode = {
    blockLike.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>
        // Convert contained ports and blocks to HGraph/Edgir* objects
        val portMembers: Map[String, EdgirNodeMember] =
          block.ports.mapValues(port => portLikeToPort(port, lib))
        val blockMembers: Map[String, EdgirNodeMember] =
          block.blocks.mapValues(subblock => blockLikeToNode(subblock, lib))

        // Unify the namespace into a member namespace
        val allMembers = (portMembers.toSeq ++ blockMembers.toSeq).groupBy(_._1).map { case (name, pairs) =>
          pairs match {
            case Seq((_, value)) => name -> value
            case values => throw new Exception(s"block contains ${values.length} conflicting members with name $name: $values")
          }
        }

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = block.constraints.collect { case (name, constr) =>
          constr.expr match {
            case expr.ValueExpr.Expr.Connected(connect) =>
              // in the loading pass, the source is the block side and the target is the link side
              EdgirEdge(name,
                source=EdgirUtils.RefExprToSeqString(connect.blockPort.get),
                target=EdgirUtils.RefExprToSeqString(connect.linkPort.get))
            case expr.ValueExpr.Expr.Exported(export) =>
              // in the loading pass, the source is the block side and the target is the external port
              EdgirEdge(name,
                source=EdgirUtils.RefExprToSeqString(export.internalBlockPort.get),
                target=EdgirUtils.RefExprToSeqString(export.exteriorPort.get))

          }
        }.toSeq
        EdgirNode(BlockWrapper(blockLike), allMembers, edges)
      case elem.BlockLike.Type.LibElem(block) =>
        // TODO implement me
        EdgirNode(BlockWrapper(blockLike), Map(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(BlockWrapper(blockLike), Map(), Seq())
    }
  }

  def portLikeToPort(portLike: elem.PortLike, lib: EdgirLibrary): EdgirPort = {
    // TODO implement me
    EdgirPort(portLike)
  }
}
