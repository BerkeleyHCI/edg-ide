package edg_ide.edgir_graph

import scala.collection.SeqMap
import edgir.elem.elem
import edgir.expr.expr
import edg.wir.DesignPath
import edg.EdgirUtils.SimpleLibraryPath
import edg.ExprBuilder.Ref
import edg.util.MapUtils
import edg.wir.ProtoUtil._


// Should be an union type, but not supported in Scala, so here's wrappers =(
sealed trait NodeDataWrapper {
  def path: DesignPath
}

case class BlockWrapper(path: DesignPath, blockLike: elem.BlockLike) extends NodeDataWrapper {
  override def toString: String = blockLike.`type` match {
    case elem.BlockLike.Type.Hierarchy(block) =>
      block.getSelfClass.toSimpleString
    case elem.BlockLike.Type.LibElem(lib) =>
      s"lib: ${lib.toSimpleString}"
    case other => other.getClass.getName
  }
}

case class LinkWrapper(path: DesignPath, linkLike: elem.LinkLike) extends NodeDataWrapper {
  override def toString: String = linkLike.`type` match {
    case elem.LinkLike.Type.Link(link) =>
      link.getSelfClass.toSimpleString
    case elem.LinkLike.Type.Array(link) =>
      s"${link.getSelfClass.toSimpleString}[${link.ports.size}]"
    case elem.LinkLike.Type.LibElem(lib) =>
      s"lib: ${lib.toSimpleString}"
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
    override val members: SeqMap[Seq[String], EdgirNodeMember],
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
  protected def constraintsToEdges(path: DesignPath, constraints: SeqMap[String, expr.ValueExpr]): Seq[EdgirEdge] = {
    constraints.flatMap { case (name, constr) =>
      constr.expr match {
        case expr.ValueExpr.Expr.Connected(connect) =>
          // in the loading pass, the source is the block side and the target is the link side
          Some(EdgirEdge(ConnectWrapper(path + name, constr),
            source=Ref.unapply(connect.getBlockPort.getRef).get,
            target=Ref.unapply(connect.getLinkPort.getRef).get))
        case expr.ValueExpr.Expr.Exported(export) =>
          // in the loading pass, the source is the block side and the target is the external port
          Some(EdgirEdge(ConnectWrapper(path + name, constr),
            source=Ref.unapply(export.getInternalBlockPort.getRef).get,
            target=Ref.unapply(`export`.getExteriorPort.getRef).get))
        case _ => None
      }
    }.toSeq
  }

  def blockLikeToNode(path: DesignPath, blockLike: elem.BlockLike): EdgirNode = {
    blockLike.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = MapUtils.mergeSeqMapSafe(  // arrays not collapse
          block.ports.toSeqMap.flatMap { case (name, port) => expandPortsWithNames(path + name, Seq(name), port) },
          block.blocks.toSeqMap.map { case (name, subblock) => Seq(name) -> blockLikeToNode(path + name, subblock) },
          block.links.toSeqMap.map { case (name, sublink) => Seq(name) -> linkLikeToNode(path + name, sublink) },
        ).to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, block.constraints.toSeqMap)

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
        val allMembers = MapUtils.mergeSeqMapSafe(
          link.ports.toSeqMap.flatMap { case (name, port) => expandPortsWithNames(path + name, Seq(name), port) },  // arrays collapsed
          link.links.toSeqMap.map { case (name, sublink) => Seq(name) -> linkLikeToNode(path + name, sublink) },
        ).to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, link.constraints.toSeqMap)

        EdgirNode(LinkWrapper(path, linkLike), allMembers, edges)
      case elem.LinkLike.Type.Array(link) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = MapUtils.mergeSeqMapSafe(
          link.ports.toSeqMap.flatMap { case (name, port) => expandPortsWithNames(path + name, Seq(name), port) },  // arrays collapsed
          link.links.toSeqMap.map { case (name, sublink) => Seq(name) -> linkLikeToNode(path + name, sublink) },
        ).to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, link.constraints.toSeqMap)

        EdgirNode(LinkWrapper(path, linkLike), allMembers, edges)
      case elem.LinkLike.Type.LibElem(link) =>
        // TODO implement me
        EdgirNode(LinkWrapper(path, linkLike), SeqMap(), Seq())
      case _ =>  // create an empty error block
        EdgirNode(LinkWrapper(path, linkLike), SeqMap(), Seq())
    }
  }

  // Creates a EdgirPort from an IR PortLike
  def portLikeToPort(path: DesignPath, portLike: elem.PortLike): EdgirPort = {
    EdgirPort(PortWrapper(path, portLike))
  }

  // Creates EdgirPorts from an IR PortLike, expanding arrays
  def expandPortsWithNames(path: DesignPath, name: Seq[String], portLike: elem.PortLike): Seq[(Seq[String], EdgirPort)] = {
   portLike.is match {
     case _: elem.PortLike.Is.Port | _: elem.PortLike.Is.Bundle =>
       Seq(name -> portLikeToPort(path, portLike))
     case elem.PortLike.Is.Array(portArray) =>  // create an entry for the array itself
       // TODO option to create a port for each array elt?
       Seq(name -> portLikeToPort(path, portLike))
     case _ =>  // every other case, make a "port"
       Seq(name -> portLikeToPort(path, portLike))
   }
  }
}
