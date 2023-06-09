package edg_ide

import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edg.wir.DesignPath
import edg.ElemBuilder
import edg.wir.ProtoUtil._

object EdgirUtils {
  // TODO this entire thing needs to be broken up

  // TODO this needs better flagging
  def isCategory(blockType: ref.LibraryPath): Boolean = {
    blockType.getTarget.getName.contains("Categories")
  }

  val FootprintBlockType: ref.LibraryPath =
    ElemBuilder.LibraryPath("electronics_model.CircuitBlock.CircuitBlock")

  // TODO refactor into common utils elsewher
  def typeOfBlockLike(blockLike: elem.BlockLike): Option[ref.LibraryPath] = blockLike.`type` match {
    case elem.BlockLike.Type.Hierarchy(block) => Some(block.getSelfClass)
    case elem.BlockLike.Type.LibElem(lib)     => Some(lib)
    case _                                    => None
  }

  def typeOfPortLike(portLike: elem.PortLike): Option[ref.LibraryPath] = portLike.is match {
    case elem.PortLike.Is.Port(port)   => Some(port.getSelfClass)
    case elem.PortLike.Is.Bundle(port) => Some(port.getSelfClass)
    case elem.PortLike.Is.Array(port)  => Some(port.getSelfClass)
    case elem.PortLike.Is.LibElem(lib) => Some(lib)
    case _                             => None
  }

  def typeOfLinkLike(linkLike: elem.LinkLike): Option[ref.LibraryPath] = linkLike.`type` match {
    case elem.LinkLike.Type.Link(link)   => Some(link.getSelfClass)
    case elem.LinkLike.Type.LibElem(lib) => Some(lib)
    case _                               => None
  }

  private sealed trait ResolveTarget[+T]

  private object ResolveTarget {
    object Any extends ResolveTarget[Any]
    object Block extends ResolveTarget[elem.HierarchyBlock]
    object Port extends ResolveTarget[elem.Port]
    object Link extends ResolveTarget[elem.Link]
  }

  // Resolves the deepest block. Always succeeds - can return the root if nothing matches
  def resolveDeepestBlock(path: DesignPath, start: schema.Design): (DesignPath, elem.HierarchyBlock) = {
    ResolveDeepest
      .fromDesign(path.steps, start, ResolveTarget.Block)
      .map { case (resolvedPath, resolvedBlock) => (DesignPath(resolvedPath), resolvedBlock) }
      .get
  }

  def resolveDeepestLink(path: DesignPath, start: schema.Design): Option[(DesignPath, elem.Link)] = {
    ResolveDeepest
      .fromDesign(path.steps, start, ResolveTarget.Link)
      .map { case (resolvedPath, resolvedLink) => (DesignPath(resolvedPath), resolvedLink) }
  }

  def resolveDeepest(path: DesignPath, start: schema.Design): Option[(DesignPath, Any)] = {
    ResolveDeepest
      .fromDesign(path.steps, start, ResolveTarget.Any)
      .map { case (resolvedPath, resolved) => (DesignPath(resolvedPath), resolved) }
  }

  def resolveExact(path: DesignPath, start: schema.Design): Option[Any] = {
    resolveDeepest(path, start) match {
      case Some((resolvedPath, resolvedTarget)) if resolvedPath == path => Some(resolvedTarget)
      case _                                                            => None
    }
  }

  def resolveExactBlock(path: DesignPath, start: schema.Design): Option[elem.HierarchyBlock] = {
    resolveDeepestBlock(path, start) match {
      case (resolvedPath, resolvedBlock) if resolvedPath == path => Some(resolvedBlock)
      case _                                                     => None
    }
  }

  def resolveExactLink(path: DesignPath, start: schema.Design): Option[elem.Link] = {
    resolveDeepestLink(path, start) match {
      case Some((resolvedPath, resolvedLink)) if resolvedPath == path => Some(resolvedLink)
      case _                                                          => None
    }
  }

  /** These helper functions resolve a path (given as a Seq[String]) to the deepest of some target element
    * type. This guarantees a return of the target element type (or return of None), but does not guarantee
    * the rest of the unresolved path is resolvable (to anything).
    */
  private object ResolveDeepest {
    val followIntoBlock = Set(ResolveTarget.Any, ResolveTarget.Block, ResolveTarget.Link, ResolveTarget.Port)
    val followIntoLink = Set(ResolveTarget.Any, ResolveTarget.Link, ResolveTarget.Port)
    val followIntoPort = Set(ResolveTarget.Any, ResolveTarget.Port)

    def fromDesign[T](
        postfix: Seq[String],
        design: schema.Design,
        target: ResolveTarget[T]
    ): Option[(Seq[String], T)] = {
      val topBlock = design.contents.getOrElse(elem.HierarchyBlock())
      fromBlock(Seq(), postfix, topBlock, target)
    }

    def fromBlock[T](
        prefix: Seq[String],
        postfix: Seq[String],
        block: elem.HierarchyBlock,
        target: ResolveTarget[T]
    ): Option[(Seq[String], T)] = {
      ((postfix, target): @unchecked) match {
        case (Seq(), ResolveTarget.Any | ResolveTarget.Block) => Some((prefix, block.asInstanceOf[T]))
        case (Seq(), _) => None // target isn't the right type, but nowhere to continue
        case (Seq(head, tail @ _*), target) =>
          if (block.blocks.toSeqMap.contains(head) && followIntoBlock.contains(target)) {
            fromBlockLike(prefix :+ head, tail, block.blocks(head), target)
          } else if (block.links.toSeqMap.contains(head) && followIntoLink.contains(target)) {
            fromLinkLike(prefix :+ head, tail, block.links(head), target)
          } else if (block.ports.toSeqMap.contains(head) && followIntoPort.contains(target)) {
            fromPortLike(prefix :+ head, tail, block.ports(head), target)
          } else if (target == ResolveTarget.Any || target == ResolveTarget.Block) { // deepest possible target along path
            Some(prefix, block.asInstanceOf[T])
          } else {
            None
          }
      }
    }

    def fromBlockLike[T](
        prefix: Seq[String],
        postfix: Seq[String],
        blockLike: elem.BlockLike,
        target: ResolveTarget[T]
    ): Option[(Seq[String], T)] = {
      blockLike.`type` match {
        case elem.BlockLike.Type.Hierarchy(block) => fromBlock(prefix, postfix, block, target)
        case _                                    => None
      }
    }

    def fromLinkLike[T](
        prefix: Seq[String],
        postfix: Seq[String],
        linkLike: elem.LinkLike,
        target: ResolveTarget[T]
    ): Option[(Seq[String], T)] = {
      linkLike.`type` match {
        case elem.LinkLike.Type.Link(link) =>
          ((postfix, target): @unchecked) match {
            case (Seq(), ResolveTarget.Any | ResolveTarget.Link) => Some((prefix, link.asInstanceOf[T]))
            case (Seq(), _) => None // target isn't the right type, but nowhere to continue
            case (Seq(head, tail @ _*), target) =>
              if (link.links.toSeqMap.contains(head) && followIntoLink.contains(target)) {
                fromLinkLike(prefix :+ head, tail, link.links(head), target)
              } else if (link.ports.toSeqMap.contains(head) && followIntoPort.contains(target)) {
                fromPortLike(prefix :+ head, tail, link.ports(head), target)
              } else if (target == ResolveTarget.Any || target == ResolveTarget.Link) { // deepest possible target along path
                Some(prefix, link.asInstanceOf[T])
              } else {
                None
              }
          }
        case _ => None
      }
    }

    def fromPortLike[T](
        prefix: Seq[String],
        postfix: Seq[String],
        portLike: elem.PortLike,
        target: ResolveTarget[T]
    ): Option[(Seq[String], T)] = {
      portLike.is match {
        case elem.PortLike.Is.Port(port) =>
          (postfix, target) match {
            case (_, ResolveTarget.Any | ResolveTarget.Port) =>
              Some((prefix, port.asInstanceOf[T])) // deepest along path
            case _ => None
          }
        case elem.PortLike.Is.Array(port) =>
          (postfix, target) match {
            case (Seq(), ResolveTarget.Any)  => Some((prefix, port.asInstanceOf[T]))
            case (Seq(), ResolveTarget.Port) => ??? // TODO return non-Port PortType
            case (Seq(), _)                  => None // target isn't the right type, but nowhere to continue
            case (Seq(head, tail @ _*), target) =>
              if (
                port.contains.ports
                  .getOrElse(elem.PortArray.Ports())
                  .ports
                  .toSeqMap
                  .contains(
                    head
                  ) && target == ResolveTarget.Port
              ) {
                fromPortLike(prefix :+ head, tail, port.getPorts.ports(head), target)
              } else if (target == ResolveTarget.Port) { // deepest possible target along path
                ??? // TODO return non-Port PortType
              } else if (target == ResolveTarget.Any) { // deepest possible target along path
                Some((prefix, port.asInstanceOf[T]))
              } else {
                None
              }
            case _ => None
          }
        case elem.PortLike.Is.Bundle(port) =>
          (postfix, target) match {
            case (Seq(), ResolveTarget.Any)  => Some((prefix, port.asInstanceOf[T]))
            case (Seq(), ResolveTarget.Port) => ??? // TODO return non-Port PortType
            case (Seq(), _)                  => None // target isn't the right type, but nowhere to continue
            case (Seq(head, tail @ _*), target) =>
              if (port.ports.toSeqMap.contains(head) && target == ResolveTarget.Port) {
                fromPortLike(prefix :+ head, tail, port.ports(head), target)
              } else if (target == ResolveTarget.Port) { // deepest possible target along path
                ??? // TODO return non-Port PortType
              } else if (target == ResolveTarget.Any) { // deepest possible target along path
                Some((prefix, port.asInstanceOf[T]))
              } else {
                None
              }
            case _ => None
          }
        case _ => None
      }
    }
  }
}
