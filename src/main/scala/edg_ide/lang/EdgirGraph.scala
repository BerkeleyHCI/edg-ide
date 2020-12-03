package edg_ide

import edg.elem.elem
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
        val portMembers = block.ports.mapValues(port => portLikeToPort(port, lib))
        val blockMembers = block.blocks.mapValues(subblock => blockLikeToNode(subblock, lib))
        val allMembers: Map[String, EdgirNodeMember] = Map()
        val edges: Seq[EdgirEdge] = Seq()  // TODO implement me
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
