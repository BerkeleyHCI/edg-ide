package edg_ide.util
import edg.wir.ProtoUtil.{BlockProtoToSeqMap, ConstraintProtoToSeqMap, PortProtoToSeqMap}
import edgir.elem.elem
import edgir.expr.expr

import scala.collection.mutable

// provides link-level connectivity information (e.g. all connected ports in a link) for a block
class BlockConnectedAnalysis(val block: elem.HierarchyBlock) {
  protected val connectionsBuilder = mutable.ArrayBuffer[(
      Option[String], // link name, if part of a link
      mutable.ArrayBuffer[PortConnectTyped[PortConnects.ConstraintBase]],
      mutable.ArrayBuffer[expr.ValueExpr]
  )]()
  protected val linkNameToConnectionIndex = mutable.Map[String, Int]() // allows quick indexing

  // here, invalid constraints are silently discarded
  block.constraints.toSeqMap.foreach { case (name, constr) =>
    val linkNameOpt = constr.expr match { // get the link name / builder map key, if it is a valid constraint
      case expr.ValueExpr.Expr.Connected(connected) =>
        Some(connected.getLinkPort.getRef.steps.head.getName)
      case expr.ValueExpr.Expr.ConnectedArray(connected) =>
        Some(connected.getLinkPort.getRef.steps.head.getName)
      case expr.ValueExpr.Expr.Exported(exported) => None
      case expr.ValueExpr.Expr.ExportedArray(exported) => None
      case _ => None // ignored
    }
    val (connectedBuilder, constrBuilder) = linkNameOpt match {
      case Some(linkName) => // is a link, need to fetch the existing link entry or add a new one
        linkNameToConnectionIndex.get(linkName) match {
          case Some(index) =>
            (connectionsBuilder(index)._2, connectionsBuilder(index)._3)
          case None =>
            connectionsBuilder.append((Some(linkName), mutable.ArrayBuffer(), mutable.ArrayBuffer()))
            linkNameToConnectionIndex(linkName) = connectionsBuilder.length - 1
            (connectionsBuilder.last._2, connectionsBuilder.last._3)
        }
      case None => // anonymous
        connectionsBuilder.append((None, mutable.ArrayBuffer(), mutable.ArrayBuffer()))
        (connectionsBuilder.last._2, connectionsBuilder.last._3)
    }

    val connectedPortsOpt = PortConnects.fromConnect(constr).map { connecteds =>
      // silently discard non-found port
      connecteds.flatMap(PortConnectTyped.fromConnect(_, block))
    }
    connectedPortsOpt.foreach { connectedPorts =>
      connectedBuilder.addAll(connectedPorts)
    }
    constrBuilder.append(constr)
  }

  protected val allConnectedPorts = connectionsBuilder.flatMap { case (name, connecteds, constrs) =>
    connecteds.map(_.connect.topPortRef)
  }.toSet

  protected val disconnectedBoundaryPortConnections =
    mutable.ArrayBuffer[PortConnectTyped[PortConnects.ConstraintBase]]()
  // TODO boundary ports, exports, and bridging currently not supported
//  block.ports.toSeqMap.collect {
//    case (portName, port) if !allConnectedPorts.contains(Seq(portName)) =>
//      val connectTypedOpt = port.is match {
//        case elem.PortLike.Is.Port(port) =>
//          Some(PortConnectTyped(PortConnects.BoundaryPort(portName, Seq()), port.getSelfClass))
//        case elem.PortLike.Is.Bundle(port) =>
//          Some(PortConnectTyped(PortConnects.BoundaryPort(portName, Seq()), port.getSelfClass))
//        case elem.PortLike.Is.Array(array) =>
//          Some(PortConnectTyped(PortConnects.BoundaryPortVectorUnit(portName), array.getSelfClass))
//        case _ => None
//      }
//      connectTypedOpt.foreach { connect =>
//        disconnectedBoundaryPortConnections.append(connect)
//      }
//  }

  protected val disconnectedBlockPortConnections =
    mutable.ArrayBuffer[PortConnectTyped[PortConnects.ConstraintBase]]()
  block.blocks.toSeqMap.foreach { case (subBlockName, subBlock) =>
    subBlock.`type`.hierarchy.foreach { subBlock =>
      subBlock.ports.toSeqMap.map { case (name, port) => (name, port.is) }.foreach {
        case (subBlockPortName, elem.PortLike.Is.Port(port))
          if !allConnectedPorts.contains(Seq(subBlockName, subBlockPortName)) =>
          disconnectedBlockPortConnections.append(
            PortConnectTyped(PortConnects.BlockPort(subBlockName, subBlockPortName), port.getSelfClass)
          )
        case (subBlockPortName, elem.PortLike.Is.Bundle(port))
          if !allConnectedPorts.contains(Seq(subBlockName, subBlockPortName)) =>
          disconnectedBlockPortConnections.append(
            PortConnectTyped(PortConnects.BlockPort(subBlockName, subBlockPortName), port.getSelfClass)
          )
        case (subBlockPortName, elem.PortLike.Is.Array(array))
          if !allConnectedPorts.contains(Seq(subBlockName, subBlockPortName)) =>
          disconnectedBlockPortConnections.append(
            PortConnectTyped(PortConnects.BlockVectorUnit(subBlockName, subBlockPortName), array.getSelfClass)
          )
        case _ => None
      }
    }
  }

  // returns all connections for this block, each connection being the link name (if part of a link),
  // ports attached (as ConnectTypes.Base), and list of constraints
  // disconnected ports returned as a single port of BlockPort (for block ports), BoundaryPort (for boundary ports),
  // and TBD for vectors (currently includes a new slice, if in a slice connection or disconnected)
  val connectedGroups: Seq[(Option[String], Seq[PortConnectTyped[PortConnects.ConstraintBase]], Seq[expr.ValueExpr])] =
    connectionsBuilder.toSeq.map { case (linkNameOpt, connecteds, constrs) =>
      (linkNameOpt, connecteds.toSeq, constrs.toSeq)
    } ++ (disconnectedBoundaryPortConnections ++ disconnectedBlockPortConnections).map { connected =>
      (None, Seq(connected), Seq())
    }

  // returns a list of active bridges, as the boundary port and the block port (link-facing bridge port)
  // bridge arrays are not (current) a construct and not supported
  // TODO: infer bridges and add to separate structure
  val bridges: Seq[(PortConnects.BoundaryPort, PortConnects.BlockPort)] = Seq() // TODO IMPLEMENT ME
}
