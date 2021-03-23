package edg_ide.ui
import edg.IrPort
import edg.elem.elem
import edg.ref.ref
import edg.util.Errorable
import edg.util.SeqMapSortableFrom.MapSortableFrom
import edg.wir
import edg.wir.{BlockConnectivityAnalysis, LibraryConnectivityAnalysis, ProtoUtil}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption}
import edg_ide.util.{ExceptionNotifyException, exceptable}
import edg.ExprBuilder.Ref


/** Utility methods for fast-path modifications to the design from a UI action.
  */
class DesignFastPathUtil(library: wir.Library) {
  lazy val libraryAnalysis = new LibraryConnectivityAnalysis(library)

  def instantiatePortLike(portType: ref.LibraryPath): Errorable[elem.PortLike] = exceptable {
    library.getPort(portType).exceptError match {
      case IrPort.Port(port) => elem.PortLike().update(
        _.port := port.update(
          _.superclasses := Seq(portType)
        )
      )
      case IrPort.Bundle(bundle) => elem.PortLike().update(
        _.bundle := bundle.update(
          _.superclasses := Seq(portType)
        )
      )
    }
  }

  private def recursiveExpandPort(portLike: elem.PortLike): Errorable[elem.PortLike] = exceptable {
    portLike.is match {
      case elem.PortLike.Is.LibElem(portType) => instantiatePortLike(portType).exceptError
      case elem.PortLike.Is.Array(array) => portLike  // for now, don't elaborate arrays
      case other => throw ExceptionNotifyException(s"unexpected ${other.getClass} in expand port")
    }

  }

  /** Return a stub block, with the proper superclass, single-level-deep ports,
    * but no links, subblocks, or constraints.
    */
  def instantiateStubBlock(blockType: ref.LibraryPath): Errorable[elem.HierarchyBlock] = exceptable {
    val newBlock = library.getBlock(blockType).exceptError
    newBlock.update(  // create a placeholder only
      _.superclasses := Seq(blockType),
      _.ports := newBlock.ports.mapValues(recursiveExpandPort)
          .mapValues(_.exceptError).toMap,
      _.blocks := Map(),
      _.links := Map(),
      _.constraints := newBlock.constraints.filter { case (name, constr) =>
        constr.expr.ref match {
          case Some(ref) => ref.steps.lastOption match {
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
    elem.BlockLike().update(
      _.hierarchy := instantiateStubBlock(blockType).exceptError
    )
  }

  /** Given the exterior facing port type,
    * returns a stub BlockLike bridge, and the type of the inner (link) facing port
    */
  def instantiateStubBridgeLike(extPortType: ref.LibraryPath): Errorable[(elem.BlockLike, ref.LibraryPath)] = exceptable {
    val bridgeType = libraryAnalysis.bridgeByOuter(extPortType).exceptNone(s"no bridge")
    val bridgeBlockLike = instantiateStubBlockLike(bridgeType).exceptError
    val bridgedPortLike = bridgeBlockLike.getHierarchy.ports(LibraryConnectivityAnalysis.portBridgeLinkPort)
    val bridgedPortType = BlockConnectivityAnalysis.typeOfPortLike(bridgedPortLike)
    (bridgeBlockLike, bridgedPortType)
  }

  /** Given all connected types as a map of some arbitrary key to port type,
    * returns a stub LinkLike link and a map of the key to the port ref
    */
  def instantiateStubLinkLike[T](connectedTypes: Map[T, ref.LibraryPath]):
      Errorable[(elem.LinkLike, Map[T, ref.LocalPath])] = exceptable {
    import collection.mutable
    import edg.ExprBuilder

    // TODO perhaps check here the link of the first port is consistent for the rest?
    val linkType = libraryAnalysis.linkOfPort(connectedTypes.head._2).exceptNone(s"no link")
    val newLink = library.getLink(linkType).exceptError
    var stubLink = newLink.update(
      _.superclasses := Seq(linkType),
      _.ports := newLink.ports.mapValues(recursiveExpandPort)
          .mapValues(_.exceptError).toMap,
      _.links := Map(),
      _.constraints := Map()
    )

    // Allocate incoming connects to link ports, and allocate port array elts as needed
    // TODO it's yet another variant of the link connect algorithm, this has been written too many times.
    // Can these all be consolidated?
    val linkSortedPorts = stubLink.ports.sortKeysFrom(ProtoUtil.getNameOrder(newLink.meta)).to(mutable.SeqMap)
    def nextOfType(findType: ref.LibraryPath): Option[(String, elem.PortLike)] = {
      linkSortedPorts.find {
        case (name, portLike) => BlockConnectivityAnalysis.typeOfPortLike(portLike) == findType
      }
    }
    val arraySize = mutable.Map[String, Int]()

    val connectMap = connectedTypes.map { case (key, connectType) =>
      val (topPortName, topPortLikeDummy) = nextOfType(connectType).exceptNone("no port type")
      topPortLikeDummy.is match {
        case elem.PortLike.Is.Port(_) | elem.PortLike.Is.Bundle(_) =>
          // return the reference and remove the port
          linkSortedPorts.remove(topPortName)
          key -> ExprBuilder.Ref(topPortName)
        case elem.PortLike.Is.Array(array) =>
          // allocate a new array port and return the reference
          val i = arraySize.getOrElse(topPortName, 0)
          arraySize.put(topPortName, i+1)
          stubLink = stubLink.update(
            _.ports(topPortName).array.ports(i.toString) := instantiatePortLike(connectType).exceptError
          )
          key -> ExprBuilder.Ref(topPortName, i.toString)
        case other => throw ExceptionNotifyException(s"unexpected ${other.getClass} in link connect mapping")
      }
    }

    val stubLinkLike = elem.LinkLike().update(
      _.link := stubLink
    )
    (stubLinkLike, connectMap)
  }
}
