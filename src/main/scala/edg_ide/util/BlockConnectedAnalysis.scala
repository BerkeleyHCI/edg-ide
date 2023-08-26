package edg_ide.util
import edg.wir.ProtoUtil.ConstraintProtoToSeqMap
import edgir.elem.elem
import edgir.expr.expr

import scala.collection.mutable

// provides link-level connectivity information (e.g. all connected ports in a link) for a block
class BlockConnectedAnalysis(block: elem.HierarchyBlock) {
  // TODO: gather all constraints by link; exports go here too as own pseudolink for consistency
  // TODO: infer bridges and add to separate structure

  // link name -> list of connected ports
  protected val linkConnectivityBuilder = mutable.SeqMap[String, mutable.ArrayBuffer[ConnectTypes.Base]]()
  protected val linkConstraintBuilder = mutable.SeqMap[String, mutable.ArrayBuffer[expr.ValueExpr]]()

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
      val connectedPortsOpt = ConnectTypes.fromConnect(constr)
      connectedPortsOpt.foreach { connectedPorts =>
        linkConnectivityBuilder.getOrElseUpdate(linkName, mutable.ArrayBuffer()).addAll(connectedPorts)
      }
      linkConstraintBuilder.getOrElseUpdate(linkName, mutable.ArrayBuffer()).append(constr)
    }
  }

  // returns all connections for this block, each connection being the ports attached (as ConnectTypes.Base)
  // and list of constraints
  // disconnected ports returned as a single port of BlockPort (for block ports), BoundaryPort (for boundary ports),
  // BlockVectorUnit (for block arrays - even if slice capable), or BoundaryPortVctorUnit (for boundary arrays)
  def connectedGroups: Seq[(Seq[ConnectTypes.Base], Seq[expr.ValueExpr])] = ???

  // returns a list of active bridges, as the boundary port and the block port (link-facing bridge port)
  // bridge arrays are not (current) a construct and not supported
  def bridges: Seq[(ConnectTypes.BoundaryPort, ConnectTypes.BlockPort)] = ???
}
