package edg_ide

import edg.elem.elem
import edg.expr.expr
import edg.ref.ref


object EdgirGraph {
  // Edgir graph is a type parameterization of the HGraphNode
  type EdgirNodeMember = HGraphNodeMember[elem.BlockLike, elem.PortLike, String]
  type EdgirNode = HGraphNode[elem.BlockLike, elem.PortLike, String]
  val EdgirNode = HGraphNode
  type EdgirPort = HGraphPort[elem.PortLike]
  val EdgirPort = HGraphPort
  type EdgirEdge = HGraphEdge[String]
  val EdgirEdge = HGraphEdge

  def blockToNode(block: elem.HierarchyBlock, lib: EdgirLibrary): EdgirNode = {
    // TODO implement me
    EdgirNode(elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(block)), Map(), Seq())
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
            case (_, value) :: Nil => (name -> value)
            case _ => throw new Exception(s"block contains conflicting members with name $name")
          }
        }

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = block.constraints.collect { case (name, constr) =>
          constr.expr match {
            case expr.ValueExpr.Expr.Connected(connect) =>
            case expr.ValueExpr.Expr.Exported(export) =>
          }
        }
        EdgirNode(blockLike, allMembers, edges)
      case elem.BlockLike.Type.LibElem(block) =>
        // TODO implement me
        EdgirNode(blockLike, Map(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(blockLike, Map(), Seq())
    }
  }

  def portLikeToPort(portLike: elem.PortLike, lib: EdgirLibrary): EdgirPort = {
    // TODO implement me
    EdgirPort(portLike)
  }
}
