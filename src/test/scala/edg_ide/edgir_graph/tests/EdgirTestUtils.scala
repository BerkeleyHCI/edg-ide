package edg_ide.edgir_graph.tests

import edg_ide.EdgirUtils
import edg.elem.elem
import edg.expr.expr
import edg_ide.edgir_graph
import edg_ide.edgir_graph.EdgirGraph


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
    val LinkLike = elem.LinkLike()
    val LinkWrapper = edgir_graph.LinkWrapper(
      elem.LinkLike(`type`=elem.LinkLike.Type.Link(Link)))

    val Port = elem.Port()
    val PortWrapper = edgir_graph.PortWrapper(
      elem.PortLike(is=elem.PortLike.Is.Port(Port)))

    def ConnectWrapper(name: String) = edgir_graph.ConnectWrapper(
      name, expr.ValueExpr())
  }

  object TestGraphs {
    /**
      * Test graph containing a source connected to a sink through a link
      */
    val flatGraph = EdgirGraph.EdgirNode(
      data = EdgirTestUtils.Dummy.BlockWrapper,
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper,
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper,
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper,
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper("connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper("connect_sink"),
          source = Seq("sink", "port"),
          target = Seq("link", "sinks")
        ),
      )
    )

    /**
      * Test graph containing the top-level flat graph (with edges, as above)
      * but with both the source and sink containing an inner export
      */
    val hierarchyGraph = EdgirGraph.EdgirNode(
      data = EdgirTestUtils.Dummy.BlockWrapper,
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper,
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = EdgirTestUtils.Dummy.BlockWrapper,
              members = Map(
                "port" -> EdgirGraph.EdgirPort(
                  data = EdgirTestUtils.Dummy.PortWrapper
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = EdgirTestUtils.Dummy.ConnectWrapper("export_inner"),
              source = Seq("inner", "port"),
              target = Seq("port")
            )
          )
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper,
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = EdgirTestUtils.Dummy.BlockWrapper,
              members = Map(
                "port" -> EdgirGraph.EdgirPort(
                  data = EdgirTestUtils.Dummy.PortWrapper
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = EdgirTestUtils.Dummy.ConnectWrapper("export_inner"),
              source = Seq("inner", "port"),
              target = Seq("port")
            )
          )
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper,
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper("connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper("connect_sink"),
          source = Seq("sink", "port"),
          target = Seq("link", "sinks")
        ),
      )
    )
  }
}
