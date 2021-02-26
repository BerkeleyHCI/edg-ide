package edg_ide

import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.wir.DesignPath

import scala.annotation.tailrec


object EdgirUtils {
  @deprecated("use ExprBuilder")
  def StringToLibraryPath(path: String): ref.LibraryPath = {
    ref.LibraryPath(target=Some(ref.LocalStep(step=ref.LocalStep.Step.Name(path))))
  }

  @deprecated("use ExprBuilder or similar?")
  def LibraryPathToString(path: ref.LibraryPath): String = {
    // TODO handle LibraryName and Namespace
    path.target match {
      case Some(target) => target.step match {
        case ref.LocalStep.Step.Name(step) => step
        case ref.LocalStep.Step.ReservedParam(step) => step.toString
        case ref.LocalStep.Step.Empty => "(empty LocalStep)"
      }
      case None => "(LibraryPath with no target)"
    }
  }

  def SimpleLibraryPath(path: ref.LibraryPath): String = {
    // TODO once namespaces are handled properly, this should use that instead of string ops
    LibraryPathToString(path).split('.').last
  }

  def SimpleSuperclass(superclasses: Seq[ref.LibraryPath]): String = {
    superclasses.map(SimpleLibraryPath).mkString(", ")
  }

  /**
    * Converts a ValueExpr containing a LocalPath ref to a Seq[String] of the ref's components
    * Errors out with an exception if things aren't just right
    * TODO: cleaner error handling?
    */
  @deprecated("use ExprBuilder / unapply")
  def RefExprToSeqString(valueExpr: expr.ValueExpr): Seq[String] = {
    valueExpr.expr match {
      case expr.ValueExpr.Expr.Ref(refExpr) => refExpr.steps.map { step =>
        step.step match {
          case ref.LocalStep.Step.Name(name) => name
          case ref.LocalStep.Step.ReservedParam(reservedParam) =>
            throw new Exception(s"Expected path type, got unexpected reserved_param $reservedParam, in ValueExpr $valueExpr")
          case other =>
            throw new Exception(s"Expected path type, got unexpected step $other, in ValueExpr $valueExpr")
        }
      }
      case _ => throw new Exception(s"Expected path type for ValueExpr $valueExpr")
    }
  }

  /**
    * Converts a Seq[String] to a LocalPath ValueExpr, where each element in the input seq is
    * treated as a discrete LocalStep
    */
  @deprecated("use ExprBuilder")
  def SeqStringToRefExpr(path: Seq[String]): expr.ValueExpr = {
    expr.ValueExpr(
      expr=expr.ValueExpr.Expr.Ref(ref.LocalPath(
        steps=path.map { pathElt =>
          ref.LocalStep(step=ref.LocalStep.Step.Name(pathElt))
        }
      ))
    )
  }

  @deprecated("Use resolveFrom*Like")
  def ResolvePath(start: elem.HierarchyBlock, path: Seq[String]): Option[Any] = path match {  // TODO should be Union
    case Seq(head, tail@_*) =>
      start.blocks.get(head) match {
        case Some(child) => child.`type` match {
          case elem.BlockLike.Type.Hierarchy(child) => return ResolvePath(child, tail)
        }
        case _ =>  // fall through
      }
      start.ports.get(head) match {
        case Some(child) => child.is match {
          case elem.PortLike.Is.Port(child) if tail.isEmpty => return Some(child)
          // fail noisily - shouldn't happen
        }
        case _ =>  // fall through
      }
      None
    case Seq() => Some(start)
  }

  // Resolves to a *Like from a Block. If path is root, this creates a dummy top-level BlockLike.
  def resolveFromBlock(path: DesignPath, block: elem.HierarchyBlock): Option[Any] = {
    resolveFromBlockLike(path.steps, elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(block)))
  }

  def resolveBlockFromBlock(path: DesignPath, block: elem.HierarchyBlock): Option[elem.HierarchyBlock] = {
    resolveFromBlock(path, block) match {
      case Some(blockLike: elem.BlockLike) => blockLike.`type` match {
        case elem.BlockLike.Type.Hierarchy(block) => Some(block)
        case _ => None
      }
      case _ => None
    }
  }

  @tailrec
  private def resolveFromBlockLike(path: Seq[String], blockLike: elem.BlockLike): Option[Any] = (path, blockLike.`type`) match {
    case (Seq(), _) => Some(blockLike)
    case (Seq(head, tail@_*), elem.BlockLike.Type.Hierarchy(block)) =>
      if (block.ports.contains(head)) {
        resolveFromPortLike(tail, block.ports(head))
      } else if (block.blocks.contains(head)) {
        resolveFromBlockLike(tail, block.blocks(head))
      } else if (block.links.contains(head)) {
        resolveFromLinkLike(tail, block.links(head))
      } else {
        None
      }
    case _ => None
  }

  @tailrec
  private def resolveFromPortLike(path: Seq[String], portLike: elem.PortLike): Option[Any] = (path, portLike.is) match {
    case (Seq(), _) => Some(portLike)
    case (Seq(head, tail@_*), elem.PortLike.Is.Port(port)) =>
      None
    case (Seq(head, tail@_*), elem.PortLike.Is.Bundle(port)) =>
      if (port.ports.contains(head)) {
        resolveFromPortLike(tail, port.ports(head))
      } else {
        None
      }
    case (Seq(head, tail@_*), elem.PortLike.Is.Array(port)) =>
      if (port.ports.contains(head)) {
        resolveFromPortLike(tail, port.ports(head))
      } else {
        None
      }
    case _ => None
  }

  @tailrec
  private def resolveFromLinkLike(path: Seq[String], linkLike: elem.LinkLike): Option[Any] = (path, linkLike.`type`) match {
    case (Seq(), _) => Some(linkLike)
    case (Seq(head, tail@_*), elem.LinkLike.Type.Link(link)) =>
      if (link.ports.contains(head)) {
        resolveFromPortLike(tail, link.ports(head))
      } else if (link.links.contains(head)) {
        resolveFromLinkLike(tail, link.links(head))
      } else {
        None
      }
    case _ => None
  }
}

