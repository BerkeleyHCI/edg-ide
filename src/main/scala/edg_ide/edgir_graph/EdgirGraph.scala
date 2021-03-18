package edg_ide.edgir_graph

import edg.elem.elem
import edg.expr.expr
import edg_ide.EdgirUtils
import edg.wir.DesignPath

import scala.collection.SeqMap


// Should be an union type, but not supported in Scala, so here's wrappers =(
sealed trait NodeDataWrapper {
  def path: DesignPath
}

case class BlockWrapper(path: DesignPath, blockLike: elem.BlockLike) extends NodeDataWrapper {
  override def toString: String = blockLike.`type` match {
    case elem.BlockLike.Type.Hierarchy(block) =>
      EdgirUtils.SimpleSuperclass(block.superclasses)
    case elem.BlockLike.Type.LibElem(lib) =>
      s"lib: ${EdgirUtils.LibraryPathToString(lib)}"
    case other => other.getClass.getName
  }
}

case class LinkWrapper(path: DesignPath, linkLike: elem.LinkLike) extends NodeDataWrapper {
  override def toString: String = linkLike.`type` match {
    case elem.LinkLike.Type.Link(link) =>
      EdgirUtils.SimpleSuperclass(link.superclasses)
    case elem.LinkLike.Type.LibElem(lib) =>
      s"lib: ${EdgirUtils.LibraryPathToString(lib)}"
    case other => other.getClass.getName
  }
}

case class PortWrapper(path: DesignPath, portLike: elem.PortLike) {
  override def toString: String = ""  // don't print port types
}


sealed trait EdgeWrapper {
  def path: DesignPath
}

case class ConnectWrapper(path: DesignPath, constraint: expr.ValueExpr) extends EdgeWrapper
case class EdgeLinkWrapper(path: DesignPath, linkLike: elem.LinkLike) extends EdgeWrapper


object EdgirGraph {
  // These are type parameterization of the HGraphNode
  sealed trait EdgirNodeMember extends HGraphNodeMember[NodeDataWrapper, PortWrapper, EdgeWrapper] {
  }

  case class EdgirNode(
    override val data: NodeDataWrapper,
    override val members: SeqMap[String, EdgirNodeMember],
    override val edges: Seq[EdgirEdge]
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
  def blockToNode(path: DesignPath, block: elem.HierarchyBlock): EdgirNode = {
    blockLikeToNode(path,
      elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(block)))
  }

  /**
    * For a list of constraints, returns the EdgirEdges of corresponding connect and exports
    */
  protected def constraintsToEdges(path: DesignPath, constraints: Map[String, expr.ValueExpr]): Seq[EdgirEdge] = {
    constraints.flatMap { case (name, constr) =>
      constr.expr match {
        case expr.ValueExpr.Expr.Connected(connect) =>
          // in the loading pass, the source is the block side and the target is the link side
          Some(EdgirEdge(ConnectWrapper(path + name, constr),
            source=EdgirUtils.RefExprToSeqString(connect.blockPort.get),
            target=EdgirUtils.RefExprToSeqString(connect.linkPort.get)))
        case expr.ValueExpr.Expr.Exported(export) =>
          // in the loading pass, the source is the block side and the target is the external port
          Some(EdgirEdge(ConnectWrapper(path + name, constr),
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

  def blockLikeToNode(path: DesignPath, blockLike: elem.BlockLike): EdgirNode = {
    blockLike.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = mergeMapSafe(
          block.ports.map { case (name, port) => name -> portLikeToPort(path + name, port) },
          block.blocks.map { case (name, subblock) => name -> blockLikeToNode(path + name, subblock) },
          block.links.map{ case (name, sublink) => name -> linkLikeToNode(path + name, sublink) },
        ).to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, block.constraints)

        EdgirNode(BlockWrapper(path, blockLike), allMembers, edges)
      case elem.BlockLike.Type.LibElem(block) =>
        // TODO implement me
        EdgirNode(BlockWrapper(path, blockLike), SeqMap(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(BlockWrapper(path, blockLike), SeqMap(), Seq())
    }
  }

  def linkLikeToNode(path: DesignPath, linkLike: elem.LinkLike): EdgirNode = {
    // TODO dedup w/ blockLikeToNode
    linkLike.`type` match {
      case elem.LinkLike.Type.Link(link) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = mergeMapSafe(
          link.ports.map { case (name, port) => name -> portLikeToPort(path + name, port) },
          link.links.map { case (name, sublink) => name -> linkLikeToNode(path + name, sublink) },
        ).to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, link.constraints)

        EdgirNode(LinkWrapper(path, linkLike), allMembers, edges)
      case elem.LinkLike.Type.LibElem(link) =>
        // TODO implement me
        EdgirNode(LinkWrapper(path, linkLike), SeqMap(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(LinkWrapper(path, linkLike), SeqMap(), Seq())
    }
  }

  def portLikeToPort(path: DesignPath, portLike: elem.PortLike): EdgirPort = {
    // TODO implement me
    EdgirPort(PortWrapper(path, portLike))
  }
}
