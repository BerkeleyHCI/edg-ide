package edg_ide.util

import com.intellij.openapi.diagnostic.Logger
import edg.ElemBuilder.Constraint
import edg.ExprBuilder.Ref
import edg.util.NameCreator
import edgir.elem.elem
import edgir.expr.expr

object EdgirConnectExecutor {
  private val logger = Logger.getInstance(this.getClass)

  // modifies the Block at the IR level to add new connections
  // this is only for visualization purposes, does not need to handle constraint prop and whatnot
  def apply(
      container: elem.HierarchyBlock,
      analysis: BlockConnectedAnalysis,
      newConnected: ConnectBuilder,
      allConnects: Seq[PortConnectTyped[PortConnects.Base]]
  ): Option[elem.HierarchyBlock] = {
    if (PortConnectTyped.connectsIsExport(newConnected.connected.map(_._1))) { // export
      throw new IllegalArgumentException("TODO IMPLEMENT ME new direct export connect")
    } else { // everything else is a link
      val allConnecteds = allConnects.flatMap { connected =>
        analysis.findConnectConnectedGroupFor(connected.connect.topPortRef)
          .map { case (linkNameOpt, connecteds, constrs, connected) => (linkNameOpt, connected) }
      }
      val nonLinked = allConnecteds.collect {
        case (None, connected) => connected
      }
      val allLinks = allConnecteds.collect {
        case (Some(linkName), _) => linkName
      }.distinct
      if (allLinks.length > 1) {
        logger.warn(s"merging nets not supported: ${allLinks.distinct.mkString(", ")})}")
      }
      applyLink(container, allLinks.headOption, newConnected, nonLinked)
    }
  }

  protected def portConnectToConstraint(
      connect: PortConnectTyped[PortConnects.Base],
      connectBuilder: ConnectBuilder,
      linkName: String
  ): Option[expr.ValueExpr] = {
    val linkPortName = connectBuilder.connected.find(_._1.connect == connect.connect).map(_._2).getOrElse {
      logger.error(s"portConnectToConstraint: connect $connect not found in connected")
      return None
    }
    val constr = connect.connect match {
      // TODO: this produces a constraint that might not be valid (port arrays may not have the element, needs allocate),
      // but is good enough for the visualizer
      case PortConnects.BlockPort(blockName, portName) =>
        Constraint.Connected(Ref(blockName, portName), Ref(linkName, linkPortName))
      case PortConnects.BoundaryPort(portName, _) =>
        throw new IllegalArgumentException("TODO IMPLEMENT ME bridge connect")
      case PortConnects.BlockVectorUnit(blockName, portName) =>
        throw new IllegalArgumentException("TODO IMPLEMENT ME link array connect")
      case PortConnects.BlockVectorSlicePort(blockName, portName, _) =>
        Constraint.Connected(Ref(blockName, portName), Ref(linkName, linkPortName)) // TODO allocate on block side
      case PortConnects.BlockVectorSliceVector(blockName, portName, _) =>
        throw new IllegalArgumentException("TODO IMPLEMENT ME link array connect")
      case PortConnects.BlockVectorSlice(blockPort, portName, _) =>
        throw new IllegalArgumentException("TODO IMPLEMENT ME link array connect")
      case PortConnects.BoundaryPortVectorUnit(portName) =>
        throw new IllegalArgumentException("TODO IMPLEMENT ME bridge connect")
    }
    Some(constr)
  }

  // modifies the Block to add a link, or add connections to a link
  protected def applyLink(
      container: elem.HierarchyBlock,
      linkNameOpt: Option[String], // if None, create a new link
      newConnected: ConnectBuilder,
      newConnects: Seq[PortConnectTyped[PortConnects.Base]] // all ports to be connected to this link
  ): Option[elem.HierarchyBlock] = {
    var containerBuilder = container
    val namer = NameCreator.fromBlock(container)
    val linkName = linkNameOpt match {
      case Some(linkName) => linkName // link already exists, add to it
      case None => // no link exists, instantiate one
        val linkNewName = namer.newName("_new")
        containerBuilder = containerBuilder.update(
          _.links :+= elem.NamedLinkLike(
            name = linkNewName,
            value = Some(elem.LinkLike(elem.LinkLike.Type.Link(newConnected.linkLib)))
          )
        )
        linkNewName
    }
    val newConstraints = newConnects.flatMap { newConnect =>
      val newConstrOpt = portConnectToConstraint(newConnect, newConnected, linkName)
      newConstrOpt.map(newConstr =>
        elem.NamedValueExpr(
          name = namer.newName("_new"),
          value = Some(newConstr)
        )
      )
    }
    containerBuilder = containerBuilder.update(
      _.constraints :++= newConstraints
    )
    Some(containerBuilder)
  }
}
