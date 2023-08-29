package edg_ide.util

import com.intellij.openapi.diagnostic.Logger
import edg.EdgirUtils.SimpleLibraryPath
import edg.ExprBuilder.ValueExpr
import edg.util.SeqUtils
import edg.wir.ProtoUtil.{BlockProtoToSeqMap, PortProtoToSeqMap}
import edgir.elem.elem
import edgir.elem.elem.HierarchyBlock
import edgir.expr.expr
import edgir.ref.ref

import scala.collection.mutable

object PortConnects { // types of connections a port attached to a connection can be a part of
  // TODO materialize into constraints? - how to add tack this on to an existing IR graph
  sealed trait Base {
    def getPortType(container: elem.HierarchyBlock): Option[ref.LibraryPath] // retrieves the type from the container

    def topPortRef: Seq[String] // returns the top level port path, as (block, port) or (port) for boundary ports
  }

  // base trait for connect that can be generated from an IR constraint, as opposed to by GUI connects
  sealed trait ConstraintBase extends Base

  sealed trait PortBase extends Base // base type for any port-valued connection
  sealed trait VectorBase extends Base // base type for any vector-valued connection
  sealed trait AmbiguousBase extends Base // base type for any connection which can be port- or vector-valued

  protected def typeOfSinglePort(portLike: elem.PortLike): Option[ref.LibraryPath] = portLike.is match {
    case elem.PortLike.Is.Port(port) => port.selfClass
    case elem.PortLike.Is.Bundle(port) => port.selfClass
    case _ => None
  }

  protected def typeOfArrayPort(portLike: elem.PortLike): Option[ref.LibraryPath] = portLike.is match {
    case elem.PortLike.Is.Array(array) => array.selfClass
    case _ => None
  }

  // connects to bridges show up as this, though containing no information about the boundary port
  case class BlockPort(blockName: String, portName: String) extends PortBase with ConstraintBase {
    override def getPortType(container: HierarchyBlock): Option[ref.LibraryPath] = {
      container.blocks.toSeqMap.get(blockName).flatMap(_.`type`.hierarchy)
        .flatMap(_.ports.get(portName))
        .flatMap(typeOfSinglePort)
    }
    override def topPortRef: Seq[String] = Seq(blockName, portName)
  }

  // single exported port only
  case class BoundaryPort(portName: String, innerNames: Seq[String]) extends PortBase with ConstraintBase {
    override def getPortType(container: HierarchyBlock): Option[ref.LibraryPath] = {
      val initPort = container.ports.get(portName)
      val finalPort = innerNames.foldLeft(initPort) { case (prev, innerName) =>
        prev.flatMap(_.is.bundle)
          .flatMap(_.ports.get(innerName))
      }
      finalPort.flatMap(typeOfSinglePort)
    }
    override def topPortRef: Seq[String] = Seq(portName)
  }

  // port array, connected as a unit; port array cannot be part of any other connection
  case class BlockVectorUnit(blockName: String, portName: String) extends VectorBase with ConstraintBase {
    override def getPortType(container: HierarchyBlock): Option[ref.LibraryPath] = {
      container.blocks.toSeqMap.get(blockName).flatMap(_.`type`.hierarchy)
        .flatMap(_.ports.get(portName))
        .flatMap(typeOfArrayPort)
    }
    override def topPortRef: Seq[String] = Seq(blockName, portName)
  }

  sealed trait BlockVectorSliceBase extends Base {
    def blockName: String
    def portName: String
    override def getPortType(container: HierarchyBlock): Option[ref.LibraryPath] = { // same as BlockVectorUnit case
      container.blocks.toSeqMap.get(blockName).flatMap(_.`type`.hierarchy)
        .flatMap(_.ports.get(portName))
        .flatMap(typeOfArrayPort)
    }
    override def topPortRef: Seq[String] = Seq(blockName, portName)
  }

  // port-typed slice of a port array
  case class BlockVectorSlicePort(blockName: String, portName: String, suggestedIndex: Option[String])
      extends PortBase with BlockVectorSliceBase with ConstraintBase {}

  // vector-typed slice of a port array, connected using allocated / requested
  case class BlockVectorSliceVector(blockName: String, portName: String, suggestedIndex: Option[String])
      extends VectorBase with BlockVectorSliceBase with ConstraintBase {}

  // ambiguous slice of a port array, with no corresponding IR construct but used intuitively for GUI connections
  case class BlockVectorSlice(blockName: String, portName: String, suggestedIndex: Option[String])
      extends AmbiguousBase with BlockVectorSliceBase {}

  // port array, connected as a unit; port array cannot be part of any other connection
  case class BoundaryPortVectorUnit(portName: String) extends VectorBase with ConstraintBase {
    override def getPortType(container: HierarchyBlock): Option[ref.LibraryPath] = {
      container.ports.get(portName)
        .flatMap(typeOfArrayPort)
    }
    override def topPortRef: Seq[String] = Seq(portName)
  }

  // turns an unlowered (but optionally expanded) connect expression into a structured connect type, if the form matches
  // None means the expression failed to decode
  def fromConnect(constr: expr.ValueExpr): Option[Seq[ConstraintBase]] = constr.expr match {
    case expr.ValueExpr.Expr.Connected(connected) =>
      singleBlockPortFromRef(connected.getBlockPort).map(Seq(_))
    case expr.ValueExpr.Expr.Exported(exported) =>
      val exterior = exported.getExteriorPort match {
        case ValueExpr.Ref(Seq(portName, innerNames @ _*)) => Some(BoundaryPort(portName, innerNames))
        case _ => None // invalid / unrecognized form
      }
      val interior = singleBlockPortFromRef(exported.getInternalBlockPort)
      (exterior, interior) match {
        case (Some(exterior), Some(interior)) => Some(Seq(exterior, interior))
        case _ => None // at least one failed to decode
      }
    case expr.ValueExpr.Expr.ConnectedArray(connectedArray) =>
      vectorBlockPortFromRef(connectedArray.getBlockPort).map(Seq(_))
    case expr.ValueExpr.Expr.ExportedArray(exportedArray) =>
      val exterior = exportedArray.getExteriorPort match {
        // exported array only supported as a unit, the compiler cannot materialize subarray indices
        case ValueExpr.Ref(Seq(portName)) => Some(BoundaryPortVectorUnit(portName))
        case _ => None // invalid / unrecognized form
      }
      val interior = vectorBlockPortFromRef(exportedArray.getInternalBlockPort)
      (exterior, interior) match {
        case (Some(exterior), Some(interior)) => Some(Seq(exterior, interior))
        case _ => None // at least one failed to decode
      }
    case _ => None
  }

  protected def singleBlockPortFromRef(ref: expr.ValueExpr): Option[ConstraintBase] = ref match {
    case ValueExpr.Ref(Seq(blockName, portName)) => Some(BlockPort(blockName, portName))
    case ValueExpr.RefAllocate(Seq(blockName, portName), suggestedName) =>
      Some(BlockVectorSlicePort(blockName, portName, suggestedName))
    case _ => None // invalid / unrecognized form
  }

  protected def vectorBlockPortFromRef(ref: expr.ValueExpr): Option[ConstraintBase] = ref match {
    case ValueExpr.Ref(Seq(blockName, portName)) => Some(BlockVectorUnit(blockName, portName))
    case ValueExpr.RefAllocate(Seq(blockName, portName), suggestedName) =>
      Some(BlockVectorSliceVector(blockName, portName, suggestedName))
    case _ => None // invalid / unrecognized form
  }
}

object PortConnectTyped {
  def fromConnect[PortConnectType <: PortConnects.Base](
      connect: PortConnectType,
      container: elem.HierarchyBlock
  ): Option[PortConnectTyped[PortConnectType]] =
    connect.getPortType(container).map(portType => PortConnectTyped(connect, portType))

  // like fromConnect, but returns None if any connect could not have a type determined
  def fromConnectsAll[PortConnectType <: PortConnects.Base](
      connects: Seq[PortConnectType],
      container: elem.HierarchyBlock
  ): Option[Seq[PortConnectTyped[PortConnectType]]] =
    SeqUtils.getAllDefined(connects.map { PortConnectTyped.fromConnect(_, container) })

  // returns whether the sequence of connects with port types is a direct export
  def connectsIsExport(connects: Seq[PortConnectTyped[PortConnects.Base]]): Boolean = connects match {
    case Seq(
        PortConnectTyped(PortConnects.BoundaryPort(_, _), boundaryType),
        PortConnectTyped(PortConnects.BlockPort(_, _), blockType)
      ) if boundaryType == blockType => true
    case Seq(
        PortConnectTyped(PortConnects.BlockPort(_, _), blockType),
        PortConnectTyped(PortConnects.BoundaryPort(_, _), boundaryType)
      ) if boundaryType == blockType => true
    case _ => false
  }
}

case class PortConnectTyped[+PortConnectType <: PortConnects.Base](
    connect: PortConnectType,
    portType: ref.LibraryPath
) {}

object ConnectMode { // state of a connect-in-progress
  trait Base
  case object Port extends Base // connection between single ports, generates into link
  case object Vector extends Base // connection with at least one full vector, generates into link array
  case object Ambiguous extends Base // connection which can be either - but is ambiguous and cannot be created
}

object ConnectBuilder {
  private val logger = Logger.getInstance(this.getClass)

  // creates a ConnectBuilder given all the connects to a link (found externally) and context data
  def apply(
      container: elem.HierarchyBlock,
      linkLib: elem.Link, // link is needed to determine available connects
      constrs: Seq[expr.ValueExpr]
  ): Option[ConnectBuilder] = {
    val availableOpt = SeqUtils.getAllDefined(linkLib.ports.toSeqMap.map { case (name, portLike) =>
      (name, portLike.is)
    }
      .map { // link libraries are pre-elaboration
        case (name, elem.PortLike.Is.LibElem(port)) => Some((name, false, port))
        case (name, elem.PortLike.Is.Array(array)) => array.selfClass.map((name, true, _))
        case (name, port) =>
          logger.warn(s"unknown port type $name = ${port.getClass} in ${linkLib.getSelfClass.toSimpleString}")
          None
      }.toSeq)

    val constrConnectsOpt = SeqUtils.getAllDefined(constrs.map(PortConnects.fromConnect)).map(_.flatten)
    val constrConnectTypedOpt = constrConnectsOpt.flatMap { constrConnects =>
      PortConnectTyped.fromConnectsAll(constrConnects, container)
    }

    (availableOpt, constrConnectTypedOpt) match {
      case (Some(available), Some(constrConnectTyped)) =>
        new ConnectBuilder(linkLib, container, available, Seq(), ConnectMode.Ambiguous).append(constrConnectTyped)
      case _ =>
        if (availableOpt.isEmpty) {
          logger.warn(
            s"unable to compute available ports for ${linkLib.getSelfClass.toSimpleString} in ${container.getSelfClass.toSimpleString}"
          )
        }
        if (constrConnectTypedOpt.isEmpty) {
          logger.warn(
            s"unable to compute connected ports for ${linkLib.getSelfClass.toSimpleString} in ${container.getSelfClass.toSimpleString}"
          )
        }
        None
    }
  }
}

/** Mildly analogous to the connect builder in the frontend HDL, this starts with a link, then ports can be added.
  * Immutable, added ports return a new ConnectBuilder object (if the add was successful) or None (if the ports cannot
  * be added). Accounts for bridging and vectors. Works at the link level of abstraction, no special support for
  * bridges.
  */
class ConnectBuilder protected (
    val linkLib: elem.Link, // library
    container: elem.HierarchyBlock,
    protected val availablePorts: Seq[(String, Boolean, ref.LibraryPath)], // name, is array, port type
    val connected: Seq[(PortConnectTyped[PortConnects.Base], String)], // connect type, used port type, port name
    val connectMode: ConnectMode.Base
) {
  // Attempts to append the connections (with attached port types) to the builder, returning a new builder
  // (if successful) or None (if not a legal connect).
  def append(newConnects: Seq[PortConnectTyped[PortConnects.Base]]): Option[ConnectBuilder] = {
    val availablePortsBuilder = availablePorts.to(mutable.ArrayBuffer)
    var connectModeBuilder = connectMode
    var failedToAllocate: Boolean = false

    val newConnected = newConnects.map { connectTyped =>
      val portName = availablePortsBuilder.indexWhere(_._3 == connectTyped.portType) match {
        case -1 =>
          failedToAllocate = true
          ""
        case index =>
          val (portName, isArray, portType) = availablePortsBuilder(index)
          connectTyped.connect match {
            case _: PortConnects.PortBase =>
              if (connectModeBuilder == ConnectMode.Vector) {
                failedToAllocate = true
              } else {
                connectModeBuilder = ConnectMode.Port
              }
            case _: PortConnects.VectorBase =>
              if (connectModeBuilder == ConnectMode.Port) {
                failedToAllocate = true
              } else {
                connectModeBuilder = ConnectMode.Vector
              }
            case _: PortConnects.AmbiguousBase => // fine in either single or vector case
          }
          if (!isArray) {
            availablePortsBuilder.remove(index)
          }
          portName
      }
      (connectTyped, portName)
    }

    if (failedToAllocate) {
      None
    } else {
      Some(new ConnectBuilder(
        linkLib,
        container,
        availablePortsBuilder.toSeq,
        connected ++ newConnected,
        connectModeBuilder
      ))
    }
  }
}
