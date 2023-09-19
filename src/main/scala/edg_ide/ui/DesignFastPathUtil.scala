package edg_ide.ui
import edg.ExprBuilder.Ref
import edg.util.Errorable
import edg.wir.LibraryConnectivityAnalysis
import edg.wir.ProtoUtil._
import edg.{IrPort, wir}
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util.exceptable
import edgir.elem.elem
import edgir.ref.ref

/** Utility methods for fast-path modifications to the design from a UI action.
  */
class DesignFastPathUtil(library: wir.Library) {
  lazy val libraryAnalysis = new LibraryConnectivityAnalysis(library)

  def instantiatePortLike(portType: ref.LibraryPath): Errorable[elem.PortLike] = exceptable {
    library.getPort(portType).exceptError match {
      case IrPort.Port(port) =>
        elem
          .PortLike()
          .update(
            _.port := port
          )
      case IrPort.Bundle(bundle) =>
        elem
          .PortLike()
          .update(
            _.bundle := bundle
          )
    }
  }

  private def recursiveExpandPort(portLike: elem.PortLike): Errorable[elem.PortLike] = exceptable {
    portLike.is match {
      case elem.PortLike.Is.LibElem(portType) => instantiatePortLike(portType).exceptError
      case elem.PortLike.Is.Array(array) => portLike // for now, don't elaborate arrays
      case other => exceptable.fail(s"unexpected ${other.getClass} in expand port")
    }

  }

  /** Return a stub block, with the proper superclass, single-level-deep ports, but no links, subblocks, or constraints.
    */
  def instantiateStubBlock(blockType: ref.LibraryPath): Errorable[elem.HierarchyBlock] = exceptable {
    val newBlock = library.getBlock(blockType).exceptError
    newBlock.update( // create a placeholder only
      _.selfClass := blockType,
      _.ports := newBlock.ports.mapValues(port => recursiveExpandPort(port).exceptError),
      _.blocks := Seq(),
      _.links := Seq(),
      _.constraints := newBlock.constraints.filter {
        _.value.get.expr.ref match {
          case Some(ref) =>
            ref.steps.lastOption match {
              case Some(Ref.IsConnectedStep) => true
              case _ => false
            }
          case _ => false
        }
      }
    ) // TODO pyLib.instantiateBlock(...)?
  }

  /** Same as instantiateStubBlock, but wrapped in a BlockLike
    */
  def instantiateStubBlockLike(blockType: ref.LibraryPath): Errorable[elem.BlockLike] = exceptable {
    elem
      .BlockLike()
      .update(
        _.hierarchy := instantiateStubBlock(blockType).exceptError
      )
  }
}
