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

  protected val connectedsByPortRef = // all PortConnects by port ref, not including others in the connection
    mutable.HashMap[Seq[String], mutable.ArrayBuffer[PortConnectTyped[PortConnects.ConstraintBase]]]()
  connectionsBuilder.foreach { case (name, connecteds, constrs) =>
    connecteds.foreach { connected =>
      connectedsByPortRef.getOrElseUpdate(connected.connect.topPortRef, mutable.ArrayBuffer()).append(connected)
    }
  }

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
      subBlock.ports.toSeqMap.foreach { case (subBlockPortName, port) =>
        val disconnectedConnectOpt = port.is match {
          case elem.PortLike.Is.Port(port) if !connectedsByPortRef.contains(Seq(subBlockName, subBlockPortName)) =>
            Some(PortConnectTyped(PortConnects.BlockPort(subBlockName, subBlockPortName), port.getSelfClass))
          case elem.PortLike.Is.Bundle(port) if !connectedsByPortRef.contains(Seq(subBlockName, subBlockPortName)) =>
            Some(PortConnectTyped(PortConnects.BlockPort(subBlockName, subBlockPortName), port.getSelfClass))
          case elem.PortLike.Is.Array(array) => // arrays can have multiple connections
            val connectOpt = connectedsByPortRef.get(Seq(subBlockName, subBlockPortName)) match {
              case None => // no prior connect, add default connect
                Some(PortConnects.BlockVectorSlicePort(subBlockName, subBlockPortName, None))
              case Some(connecteds) => // prior connect, see if it's a slice and more can be appended
                if (connecteds.forall(_.connect.isInstanceOf[PortConnects.BlockVectorSlicePort])) {
                  Some(PortConnects.BlockVectorSlicePort(subBlockName, subBlockPortName, None))
                } else if (connecteds.forall(_.connect.isInstanceOf[PortConnects.BlockVectorSliceVector])) {
                  Some(PortConnects.BlockVectorSliceVector(subBlockName, subBlockPortName, None))
                } else {
                  None
                }
            }
            connectOpt.map(connect => PortConnectTyped(connect, array.getSelfClass))
          case _ => None
        }
        disconnectedConnectOpt.foreach {
          disconnectedBlockPortConnections.append
        }
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

  // returns the link name, connectedgroup, constrs, and port given a port ref
  // where multiple connectedgroups exist (for arrays), returns the one preferred when making a new connection
  // (new allocation)
  // TODO: support user-specified disambiguation of connect-as-array and connect-as-slice
  def findConnectConnectedGroupFor(portRef: Seq[String]): Option[(
      Option[String],
      Seq[PortConnectTyped[PortConnects.ConstraintBase]],
      Seq[expr.ValueExpr],
      PortConnectTyped[PortConnects.ConstraintBase]
  )] = {
    connectedGroups.findLast { case (linkNameOpt, connecteds, constrs) =>
      connecteds.exists(connected => connected.connect.topPortRef == portRef)
    }.map { case (linkNameOpt, connecteds, constrs) =>
      val portConnected = connecteds.filter(connected => connected.connect.topPortRef == portRef)
      require(portConnected.length == 1)
      (linkNameOpt, connecteds, constrs, portConnected.head)
    }
  }
}
