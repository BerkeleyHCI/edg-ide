package edg_ide.edgir_graph.tests

import edg_ide.EdgirUtils
import edg.elem.elem
import edg.expr.expr
import edg_ide.edgir_graph


object EdgirTestUtils {
  // Some definitions that need to be kept consistent with the Python HDL / frontend
  object Ports {
    val PowerSource = EdgirUtils.StringToLibraryPath("electronics_model.ElectricalPorts.ElectricalSource")
    val PowerSink = EdgirUtils.StringToLibraryPath("electronics_model.ElectricalPorts.ElectricalSink")
  }

  object Links {
    val Power = EdgirUtils.StringToLibraryPath("electronics_model.ElectricalPorts.ElectricalLink")
  }

  object Dummy {
    val Block = elem.HierarchyBlock()
    val BlockWrapper = edgir_graph.BlockWrapper(
      elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(Block)))

    val Link = elem.Link()
    val LinkWrapper = edgir_graph.LinkWrapper(
      elem.LinkLike(`type`=elem.LinkLike.Type.Link(Link)))

    val Port = elem.Port()
    val PortWrapper = edgir_graph.PortWrapper(
      elem.PortLike(is=elem.PortLike.Is.Port(Port)))

    def ConnectWrapper(name: String) = edgir_graph.ConnectWrapper(
      name, expr.ValueExpr())
  }
}
