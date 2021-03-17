package edg_ide.edgir_graph

import edg.ElemBuilder.LibraryPath

/** Removes links (as nodes - must run before they are collapsed) that are "high-fanout",
  * based on the link type allowlist and parameterized number of sink connections.
  */
object RemoveHighFanoutLinkTransform {
  private val minConnects = 4  // TODO sink only?
  private val allowedLinkTypes = Set(
    LibraryPath("electronics_model.ElectricalPorts.ElectricalLink"),
  )

  
}
