package edg_ide.util

import com.intellij.openapi.diagnostic.Logger
import edg.util.NameCreator
import edgir.elem.elem
import edgir.expr.expr

object EdgirConnectExecutor {
  // modifies the Block at the IR level to add new connections
  // this is only for visualization purposes, does not need to handle constraint prop and whatnot
  def apply(
      container: elem.HierarchyBlock,
      linkNameOpt: Option[String],
      baseConnected: ConnectBuilder,
      newConnected: ConnectBuilder,
      startingPort: PortConnectTyped[PortConnects.Base],
      newConnects: Seq[PortConnectTyped[PortConnects.Base]]
  ): Option[elem.HierarchyBlock] = {
    if (newConnects.isEmpty) { // nop
      Some(container)
    } else if (baseConnected.connected.isEmpty && PortConnectTyped.connectsIsExport(startingPort +: newConnects)) { // export
      throw new IllegalArgumentException("TODO IMPLEMENT ME new direct export connect")
    } else { // everything else is a link
      applyLink(container, linkNameOpt, baseConnected, newConnected, startingPort, newConnects)
    }
  }

  protected def portConnectToConstraint(
      connect: PortConnectTyped[PortConnects.Base],
      connectBuilder: ConnectBuilder,
      linkName: String
  ): expr.ValueExpr = {
    ???
  }

  // modifies the Block to add a link, or add connections to a link
  protected def applyLink(
      container: elem.HierarchyBlock,
      linkNameOpt: Option[String],
      baseConnected: ConnectBuilder,
      newConnected: ConnectBuilder,
      startingPort: PortConnectTyped[PortConnects.Base],
      newConnects: Seq[PortConnectTyped[PortConnects.Base]]
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
          ),
          _.constraints :+= elem.NamedValueExpr(
            name = namer.newName("_new"),
            value = Some(portConnectToConstraint(startingPort, newConnected, linkNewName))
          )
        )
        linkNewName
    }
    val newConstraints = newConnects.map { newConnect =>
      elem.NamedValueExpr(
        name = namer.newName("_new"),
        value = Some(portConnectToConstraint(startingPort, newConnected, linkName))
      )
    }
    containerBuilder = containerBuilder.update(
      _.constraints :++= newConstraints
    )
    Some(containerBuilder)
  }
}
