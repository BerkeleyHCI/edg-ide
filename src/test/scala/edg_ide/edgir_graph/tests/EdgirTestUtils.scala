package edg_ide.edgir_graph.tests

import edg_ide.EdgirUtils
import edg.elem.elem
import edg.expr.expr
import edg.wir.DesignPath
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
    def BlockWrapper(path: DesignPath) = edgir_graph.BlockWrapper(path,
      elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(Block)))

    val Link = elem.Link()
    val LinkLike = elem.LinkLike(`type` = elem.LinkLike.Type.Link(Link))
    def LinkWrapper(path: DesignPath) = edgir_graph.LinkWrapper(path,
      elem.LinkLike(`type`=elem.LinkLike.Type.Link(Link)))

    val Port = elem.Port()
    def PortWrapper(path: DesignPath) = edgir_graph.PortWrapper(path,
      elem.PortLike(is=elem.PortLike.Is.Port(Port)))

    def ConnectWrapper(path: DesignPath) = edgir_graph.ConnectWrapper(
      path, expr.ValueExpr())
  }

  object TestGraphs {
    /**
      * Test graph containing a source connected to a sink through a link
      */
    val flatGraph = EdgirGraph.EdgirNode(
      data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root),
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root + "source"),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "source" + "port")
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root + "sink"),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "sink" + "port")
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper(DesignPath.root + "link"),
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "link" + "source")
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "link" + "sinks")
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_sink"),
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
      data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root),
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root + "source"),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "source" + "port")
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root + "source" + "inner"),
              members = Map(
                "port" -> EdgirGraph.EdgirPort(
                  data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "source" + "inner" + "port")
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "source" + "export_inner"),
              source = Seq("inner", "port"),
              target = Seq("port")
            )
          )
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root + "sink"),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "sink" + "port")
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath.root + "sink" + "inner"),
              members = Map(
                "port" -> EdgirGraph.EdgirPort(
                  data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "sink" + "inner" + "port")
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "sink" + "export_inner"),
              source = Seq("inner", "port"),
              target = Seq("port")
            )
          )
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper(DesignPath.root + "link"),
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "link" + "source")
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath.root + "link" + "sinks")
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath.root + "connect_sink"),
          source = Seq("sink", "port"),
          target = Seq("link", "sinks")
        ),
      )
    )
  }
}
