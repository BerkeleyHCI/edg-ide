package edg_ide

import edg.elem.elem
import edg.ref.ref


object EdgirGraph {
  // Edgir graph is a type parameterization of the HGraphNode
  type EdgirNode = HGraphNode[elem.BlockLike, Seq[String], String]
  val EdgirNode = HGraphNode[elem.BlockLike, Seq[String], String]
  type EdgirPort = HGraphPort[elem.PortLike]
  val EdgirPort = HGraphPort[elem.PortLike]
  type EdgirEdge = HGraphEdge[String]
  val EdgirEdge = HGraphEdge[String]

  def blockLikeToNode(blockLike: elem.BlockLike, lib: EdgirLibrary): EdgirNode = {
    blockLike.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>
        val portMembers = block.ports.mapValues(port => portLikeToPort(port, lib))
        val blockMembers = block.blocks.mapValues(subblock => blockLikeToNode(subblock, lib))
        val edges =
        EdgirNode(blockLike, portMembers ++ blockMembers, edges)
      case elem.BlockLike.Type.LibElem(block) =>
      case _ =>  // create an empty error block
        EdgirNode(blockLike, Map(), Seq())
    }
  }

  def portLikeToPort(portLike: elem.PortLike, lib: EdgirLibrary): EdgirPort = {

  }
}
