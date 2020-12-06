package edg_ide.edgir_graph

import edg.elem.elem
import edg.expr.expr
import edg_ide.{EdgirLibrary, EdgirUtils}


// Should be an union type, but not supported in Scala, so here's wrappers =(
sealed trait NodeDataWrapper {
}

case class BlockWrapper(blockLike: elem.BlockLike) extends NodeDataWrapper {
  override def toString: String = blockLike.`type` match {
    case elem.BlockLike.Type.Hierarchy(block) =>
      EdgirUtils.SimpleSuperclassesToString(block.superclasses)
    case elem.BlockLike.Type.LibElem(lib) =>
      s"lib: ${EdgirUtils.LibraryPathToString(lib)}"
    case other => other.getClass.getName
  }
}

case class LinkWrapper(linkLike: elem.LinkLike) extends NodeDataWrapper {
  override def toString: String = linkLike.`type` match {
    case elem.LinkLike.Type.Link(link) =>
      EdgirUtils.SimpleSuperclassesToString(link.superclasses)
    case elem.LinkLike.Type.LibElem(lib) =>
      s"lib: ${EdgirUtils.LibraryPathToString(lib)}"
    case other => other.getClass.getName
  }
}

case class PortWrapper(portLike: elem.PortLike) {
  override def toString: String = ""  // don't print port types
}


sealed trait EdgeWrapper {
}

case class ConnectWrapper(name: String, constraint: expr.ValueExpr) extends EdgeWrapper
case class EdgeLinkWrapper(name: String, linkLike: elem.LinkLike) extends EdgeWrapper


object EdgirGraph {
  // These are type parameterization of the HGraphNode
  sealed trait EdgirNodeMember extends HGraphNodeMember[NodeDataWrapper, PortWrapper, EdgeWrapper] {
  }

  case class EdgirNode(
    override val data: NodeDataWrapper,
    override val members: Map[String, EdgirNodeMember],
    override val edges: Seq[HGraphEdge[EdgeWrapper]]
  ) extends HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper] with EdgirNodeMember {  }

  case class EdgirPort(
    override val data: PortWrapper
  ) extends HGraphPort[PortWrapper] with EdgirNodeMember {}

  case class EdgirEdge(
    override val data: EdgeWrapper,
    override val source: Seq[String],
    override val target: Seq[String]
  ) extends HGraphEdge[EdgeWrapper]

  /**
    * Simple wrapper around blockLikeToNode that provides the blockLike wrapper around the block
    */
  def blockToNode(block: elem.HierarchyBlock, name: String, lib: EdgirLibrary): EdgirNode = {
    blockLikeToNode(
      elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(block)),
      name,
      lib)
  }

  /**
    * For a list of constraints, returns the EdgirEdges of corresponding connect and exports
    */
  protected def constraintsToEdges(constraints: Map[String, expr.ValueExpr]): Seq[EdgirEdge] = {
    constraints.flatMap { case (name, constr) =>
      constr.expr match {
        case expr.ValueExpr.Expr.Connected(connect) =>
          // in the loading pass, the source is the block side and the target is the link side
          Some(EdgirEdge(ConnectWrapper(name, constr),
            source=EdgirUtils.RefExprToSeqString(connect.blockPort.get),
            target=EdgirUtils.RefExprToSeqString(connect.linkPort.get)))
        case expr.ValueExpr.Expr.Exported(export) =>
          // in the loading pass, the source is the block side and the target is the external port
          Some(EdgirEdge(ConnectWrapper(name, constr),
            source=EdgirUtils.RefExprToSeqString(export.internalBlockPort.get),
            target=EdgirUtils.RefExprToSeqString(export.exteriorPort.get)))
        case _ => None
      }
    }.toSeq
  }

  /**
    * Merges the argument maps, erroring out if there are duplicate names
    */
  protected def mergeMapSafe[T](maps: Map[String, T]*): Map[String, T] = {
    maps.flatMap(_.toSeq)  // to a list of pairs in the maps
        .groupBy(_._1)  // sort by name
        .map {
          case (name, Seq((_, value))) => name -> value
          case (name, values) => throw new Exception(s"block contains ${values.length} conflicting members with name $name: $values")
        }
  }

  def blockLikeToNode(blockLike: elem.BlockLike, name: String, lib: EdgirLibrary): EdgirNode = {
    blockLike.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = mergeMapSafe(
          block.ports.map { case (name, port) => name -> portLikeToPort(port, name, lib) },
          block.blocks.map { case (name, subblock) => name -> blockLikeToNode(subblock, name, lib) },
          block.links.map{ case (name, sublink) => name -> linkLikeToNode(sublink, name, lib) },
        )

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(block.constraints)

        EdgirNode(BlockWrapper(blockLike), allMembers, edges)
      case elem.BlockLike.Type.LibElem(block) =>
        // TODO implement me
        EdgirNode(BlockWrapper(blockLike), Map(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(BlockWrapper(blockLike), Map(), Seq())
    }
  }

  def linkLikeToNode(linkLike: elem.LinkLike, name: String, lib: EdgirLibrary): EdgirNode = {
    // TODO dedup w/ blockLikeToNode
    linkLike.`type` match {
      case elem.LinkLike.Type.Link(link) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = mergeMapSafe(
          link.ports.map { case (name, port) => name -> portLikeToPort(port, name, lib) },
          link.links.map{ case (name, sublink) => name -> linkLikeToNode(sublink, name, lib) },
        )

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(link.constraints)

        EdgirNode(LinkWrapper(linkLike), allMembers, edges)
      case elem.LinkLike.Type.LibElem(link) =>
        // TODO implement me
        EdgirNode(LinkWrapper(linkLike), Map(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(LinkWrapper(linkLike), Map(), Seq())
    }
  }

  def portLikeToPort(portLike: elem.PortLike, name: String, lib: EdgirLibrary): EdgirPort = {
    // TODO implement me
    EdgirPort(PortWrapper(portLike))
  }
}
