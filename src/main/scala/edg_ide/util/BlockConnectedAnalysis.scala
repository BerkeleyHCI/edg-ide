package edg_ide.util
import edg.wir.ProtoUtil.{BlockProtoToSeqMap, ConstraintProtoToSeqMap, PortProtoToSeqMap}
import edgir.elem.elem
import edgir.expr.expr
import edgir.ref.ref

import scala.collection.{SeqMap, mutable}

// provides link-level connectivity information (e.g. all connected ports in a link) for a block
class BlockConnectedAnalysis(block: elem.HierarchyBlock) {
  // link name -> (list of connected ports, list of constrs)
  protected val linkConnectionBuilder =
    mutable.SeqMap[
      String,
      (mutable.ArrayBuffer[PortConnectTyped[PortConnects.ConstraintBase]], mutable.ArrayBuffer[expr.ValueExpr])
    ]()

  // here, invalid constraints are silently discarded
  block.constraints.toSeqMap.foreach { case (name, constr) =>
    val linkNameOpt = constr.expr match { // get the link name / builder map key, if it is a valid constraint
      case expr.ValueExpr.Expr.Connected(connected) =>
        Some(connected.getLinkPort.getRef.steps.head.getName)
      case expr.ValueExpr.Expr.ConnectedArray(connected) =>
        Some(connected.getLinkPort.getRef.steps.head.getName)
      case expr.ValueExpr.Expr.Exported(exported) => // note, can have multiple exports to a top level port for bundles
        Some(exported.getExteriorPort.getRef.steps.head.getName)
      case expr.ValueExpr.Expr.ExportedArray(exported) =>
        Some(exported.getExteriorPort.getRef.steps.head.getName)
      case _ => None // ignored
    }
    linkNameOpt.foreach { linkName => // if the link decoded successfully
      val connectedPortsOpt = PortConnects.fromConnect(constr).map { connecteds =>
        // silently discard non-found port
        connecteds.flatMap(PortConnectTyped.fromConnect(_, block))
      }
      val (connected, constrs) =
        linkConnectionBuilder.getOrElseUpdate(linkName, (mutable.ArrayBuffer(), mutable.ArrayBuffer()))
      connectedPortsOpt.foreach { connectedPorts =>
        connected.addAll(connectedPorts)
      }
      constrs.append(constr)
    }
  }

  protected val allConnectedPorts = linkConnectionBuilder.flatMap { case (name, (connecteds, constrs)) =>
    connecteds.map(_.connect.topPortRef)
  }.toSet

  protected val disconnectedBoundaryPortConnections =
    mutable.SeqMap[String, PortConnectTyped[PortConnects.ConstraintBase]]()
  block.ports.toSeqMap.collect {
    case (portName, port) if !allConnectedPorts.contains(Seq(portName)) =>
      val connectOpt: Option[PortConnects.ConstraintBase] = port.is match {
        case elem.PortLike.Is.Port(port) => Some(PortConnects.BoundaryPort(portName, Seq()))
        case elem.PortLike.Is.Bundle(port) => Some(PortConnects.BoundaryPort(portName, Seq()))
        case elem.PortLike.Is.Array(array) => Some(PortConnects.BoundaryPortVectorUnit(portName))
        case _ => None
      }
      connectOpt.foreach { connect =>
        disconnectedBoundaryPortConnections.put(s"$portName", connect)
      }
  }

  protected val disconnectedBlockPortConnections =
    mutable.SeqMap[String, PortConnectTyped[PortConnects.ConstraintBase]]()
  block.blocks.toSeqMap.foreach { case (subBlockName, subBlock) =>
    subBlock.`type`.hierarchy.foreach { subBlock =>
      subBlock.ports.toSeqMap.collect {
        case (subBlockPortName, subBlockPort) if !allConnectedPorts.contains(Seq(subBlockName, subBlockPortName)) =>
          val connectOpt: Option[PortConnects.ConstraintBase] = subBlockPort.is match {
            case elem.PortLike.Is.Port(port) => Some(PortConnects.BlockPort(subBlockName, subBlockPortName))
            case elem.PortLike.Is.Bundle(port) => Some(PortConnects.BlockPort(subBlockName, subBlockPortName))
            case elem.PortLike.Is.Array(array) => Some(PortConnects.BlockVectorUnit(subBlockName, subBlockPortName))
            case _ => None
          }
          connectOpt.foreach { connect =>
            disconnectedBlockPortConnections.put(s"$subBlockName.$subBlockPortName", connect)
          }
      }

    }
  }

  // returns all connections for this block, each connection being the ports attached (as ConnectTypes.Base)
  // and list of constraints
  // disconnected ports returned as a single port of BlockPort (for block ports), BoundaryPort (for boundary ports),
  // BlockVectorUnit (for block arrays - even if slice capable), or BoundaryPortVectorUnit (for boundary arrays)
  val connectedGroups: SeqMap[String, (Seq[PortConnectTyped[PortConnects.ConstraintBase]], Seq[expr.ValueExpr])] =
    linkConnectionBuilder.to(SeqMap).map { case (name, (connecteds, constrs)) =>
      name -> (connecteds.toSeq, constrs.toSeq)
    } ++ (disconnectedBoundaryPortConnections ++ disconnectedBlockPortConnections).map { case (name, connected) =>
      name -> (Seq(connected), Seq())
    }

  // returns a list of active bridges, as the boundary port and the block port (link-facing bridge port)
  // bridge arrays are not (current) a construct and not supported
  // TODO: infer bridges and add to separate structure
  val bridges: Seq[(PortConnects.BoundaryPort, PortConnects.BlockPort)] = Seq() // TODO IMPLEMENT ME
}
