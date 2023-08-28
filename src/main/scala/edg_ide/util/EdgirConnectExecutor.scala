package edg_ide.util

import edgir.elem.elem

object EdgirConnectExecutor {
  // modifies the Block at the IR level to add new connections
  // this is only for visualization purposes, does not need to handle constraint prop and whatnot
  def apply(
      container: elem.HierarchyBlock,
      baseConnected: ConnectBuilder,
      newConnects: PortConnects.ConstraintBase
  ): elem.HierarchyBlock = {
    // TODO needs link name, if prior link exists?
    ???
  }
}
