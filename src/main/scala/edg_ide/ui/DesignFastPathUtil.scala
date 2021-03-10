package edg_ide.ui
import edg.IrPort
import edg.elem.elem
import edg.ref.ref
import edg.util.Errorable
import edg.wir
import edg.wir.LibraryConnectivityAnalysis
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util.exceptable


/** Utility methods for fast-path modifications to the design from a UI action.
  */
class DesignFastPathUtil(library: wir.Library) {
  lazy val libraryAnalysis = new LibraryConnectivityAnalysis(library)

  private def recursiveExpandPort(port: elem.PortLike): Errorable[elem.PortLike] = exceptable {
    library.getPort(port.getLibElem).exceptError match {  // TODO in future support block-side arrays?
      case IrPort.Port(port) => elem.PortLike().update(_.port := port)
      case IrPort.Bundle(bundle) => elem.PortLike().update(_.bundle := bundle)
    }
  }

  def instantiateStubBlock(blockType: ref.LibraryPath): Errorable[elem.HierarchyBlock] = exceptable {
    val newBlock = library.getBlock(blockType).exceptError
    newBlock.update(  // create a placeholder only
      _.superclasses := Seq(blockType),
      _.ports := newBlock.ports.mapValues(recursiveExpandPort)
          .mapValues(_.exceptError).toMap,
      _.blocks := Map(),
      _.links := Map(),
      _.constraints := Map()
    ) // TODO pyLib.instantiateBlock(...)?
  }

  def instantiateStubBlockLike(blockType: ref.LibraryPath): Errorable[elem.BlockLike] = exceptable {
    elem.BlockLike().update(
      _.hierarchy := instantiateStubBlock(blockType).exceptError
    )
  }
}
