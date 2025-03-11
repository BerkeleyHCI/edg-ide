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
      val allTypes = Seq(lib.getBase) ++ lib.mixins
      s"lib: ${allTypes.map(_.toSimpleString).mkString("+")}"
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
  override def toString: String = "" // don't print port types
}

sealed trait EdgeWrapper {
  def path: DesignPath
}

case class ConnectWrapper(path: DesignPath, constraint: expr.ValueExpr) extends EdgeWrapper
case class EdgeLinkWrapper(path: DesignPath, linkLike: elem.LinkLike) extends EdgeWrapper

object EdgirGraph {
  // These are type parameterization of the HGraphNode
  sealed trait EdgirNodeMember extends HGraphNodeMember[NodeDataWrapper, PortWrapper, EdgeWrapper] {}

  case class EdgirNode(
      override val data: NodeDataWrapper,
      override val members: SeqMap[Seq[String], EdgirNodeMember],
      override val edges: Seq[EdgirEdge]
  ) extends HGraphNode[NodeDataWrapper, PortWrapper, EdgeWrapper]
      with EdgirNodeMember {}

  case class EdgirPort(
      override val data: PortWrapper
  ) extends HGraphPort[PortWrapper]
      with EdgirNodeMember {}

  case class EdgirEdge(
      override val data: EdgeWrapper,
      override val source: Seq[String],
      override val target: Seq[String]
  ) extends HGraphEdge[EdgeWrapper]

  /** Simple wrapper around blockLikeToNode that provides the blockLike wrapper around the block
    */
  def blockToNode(path: DesignPath, block: elem.HierarchyBlock): EdgirNode = {
    blockLikeToNode(path, elem.BlockLike(`type` = elem.BlockLike.Type.Hierarchy(block)))
  }

  /** For a list of constraints, returns the EdgirEdges of corresponding connect and exports
    */
  protected def constraintsToEdges(
      path: DesignPath,
      constraints: SeqMap[String, expr.ValueExpr]
  ): Seq[EdgirEdge] = {
    constraints.flatMap { case (name, constr) =>
      constr.expr match {
        case expr.ValueExpr.Expr.Connected(connected) =>
          connectedToEdge(path, name, constr, connected)
        case expr.ValueExpr.Expr.Exported(exported) =>
          exportedToEdge(path, name, constr, exported)
        case expr.ValueExpr.Expr.ConnectedArray(connectedArray) =>
          connectedArray.expanded.flatMap(connectedToEdge(path, name, constr, _))
        case expr.ValueExpr.Expr.ExportedArray(exportedArray) =>
          exportedArray.expanded.flatMap(exportedToEdge(path, name, constr, _))
        case _ => Seq()
      }
    }.toSeq
  }

  protected def connectedToEdge(
      path: DesignPath,
      constrName: String,
      constr: expr.ValueExpr,
      connected: expr.ConnectedExpr
  ): Seq[EdgirEdge] = connected.expanded match {
    case Seq() => Seq( // in the loading pass, the source is the block side and the target is the link side
        EdgirEdge(
          ConnectWrapper(path + constrName, constr),
          source = connected.getBlockPort.getRef.steps.map(_.getName), // only block and port, ignore arrays
          target = connected.getLinkPort.getRef.steps.map(_.getName)
        ))
    case Seq(expanded) => connectedToEdge(path, constrName, constr, expanded)
    case _ => throw new IllegalArgumentException("unexpected multiple expanded")
  }

  protected def exportedToEdge(
      path: DesignPath,
      constrName: String,
      constr: expr.ValueExpr,
      exported: expr.ExportedExpr
  ): Seq[EdgirEdge] = exported.expanded match {
    case Seq() =>
      Seq( // in the loading pass, the source is the block side and the target is the external port
        EdgirEdge(
          ConnectWrapper(path + constrName, constr),
          source = exported.getInternalBlockPort.getRef.steps.map(_.getName),
          target = exported.getExteriorPort.getRef.steps.map(_.getName)
        ))
    case Seq(expanded) => exportedToEdge(path, constrName, constr, expanded)
    case _ => throw new IllegalArgumentException("unexpected multiple expanded")
  }

  def blockLikeToNode(path: DesignPath, blockLike: elem.BlockLike): EdgirNode = {
    blockLike.`type` match {
      case elem.BlockLike.Type.Hierarchy(block) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = MapUtils
          .mergeSeqMapSafe( // arrays not collapse
            block.ports.toSeqMap.flatMap { case (name, port) =>
              expandPortsWithNames(path + name, Seq(name), port)
            },
            block.blocks.toSeqMap.map { case (name, subblock) =>
              Seq(name) -> blockLikeToNode(path + name, subblock)
            },
            block.links.toSeqMap.map { case (name, sublink) =>
              Seq(name) -> linkLikeToNode(path + name, sublink)
            }
          )
          .to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, block.constraints.toSeqMap)

        EdgirNode(BlockWrapper(path, blockLike), allMembers, edges)
      case elem.BlockLike.Type.LibElem(block) =>
        // TODO implement me
        EdgirNode(BlockWrapper(path, blockLike), SeqMap(), Seq())
      case _ => // create an empty error block
        EdgirNode(BlockWrapper(path, blockLike), SeqMap(), Seq())
    }
  }

  def linkLikeToNode(path: DesignPath, linkLike: elem.LinkLike): EdgirNode = {
    // TODO dedup w/ blockLikeToNode
    linkLike.`type` match {
      case elem.LinkLike.Type.Link(link) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = MapUtils
          .mergeSeqMapSafe(
            link.ports.toSeqMap.flatMap { case (name, port) =>
              expandPortsWithNames(path + name, Seq(name), port)
            }, // arrays collapsed
            link.links.toSeqMap.map { case (name, sublink) =>
              Seq(name) -> linkLikeToNode(path + name, sublink)
            }
          )
          .to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, link.constraints.toSeqMap)

        EdgirNode(LinkWrapper(path, linkLike), allMembers, edges)
      case elem.LinkLike.Type.Array(link) =>
        // Create sub-nodes and a unified member namespace
        val allMembers = MapUtils
          .mergeSeqMapSafe(
            link.ports.toSeqMap.flatMap { case (name, port) =>
              expandPortsWithNames(path + name, Seq(name), port)
            }, // arrays collapsed
            link.links.toSeqMap.map { case (name, sublink) =>
              Seq(name) -> linkLikeToNode(path + name, sublink)
            }
          )
          .to(SeqMap)

        // Read edges from constraints
        val edges: Seq[EdgirEdge] = constraintsToEdges(path, link.constraints.toSeqMap)

        EdgirNode(LinkWrapper(path, linkLike), allMembers, edges)
      case elem.LinkLike.Type.LibElem(link) =>
        // TODO implement me
        EdgirNode(LinkWrapper(path, linkLike), SeqMap(), Seq())
      case _ => // create an empty error block
        EdgirNode(LinkWrapper(path, linkLike), SeqMap(), Seq())
    }
  }

  // Cerates a EdgirPort from an IR PortLike
  def portLikeToPort(path: DesignPath, portLike: elem.PortLike): EdgirPort = {
    EdgirPort(PortWrapper(path, portLike))
  }

  // Creates EdgirPorts from an IR PortLike, expanding arrays
  def expandPortsWithNames(
      path: DesignPath,
      name: Seq[String],
      portLike: elem.PortLike
  ): Seq[(Seq[String], EdgirPort)] = portLike.is match {
    case elem.PortLike.Is.Port(port) => Seq(name -> portLikeToPort(path, portLike))
    case elem.PortLike.Is.Bundle(port) => Seq(name -> portLikeToPort(path, portLike))
    case elem.PortLike.Is.LibElem(port) => Seq(name -> portLikeToPort(path, portLike))
    case elem.PortLike.Is.Array(array) =>
      Seq(name -> portLikeToPort(path, portLike))
      array.getPorts.ports.toSeqMap.toSeq.flatMap { case (subname, subport) =>
        expandPortsWithNames(path + subname, name :+ subname, subport)
      }
    case port => throw new NotImplementedError()
  }
}
