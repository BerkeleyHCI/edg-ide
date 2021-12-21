package edg_ide.edgir_graph.tests

import edg.ElemBuilder.LibraryPath
import edg.wir.DesignPath
import edg_ide.edgir_graph
import edg_ide.edgir_graph.EdgirGraph
import edgir.elem.elem
import edgir.expr.expr

import scala.collection.SeqMap


object EdgirTestUtils {
  // Some definitions that need to be kept consistent with the Python HDL / frontend
  object Ports {
    val PowerSource = LibraryPath("electronics_model.VoltagePorts.VoltageSource")
    val PowerSink = LibraryPath("electronics_model.VoltagePorts.VoltageSink")
  }

  object Links {
    val Power = LibraryPath("electronics_model.VoltagePorts.VoltageLink")
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
      data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath()),
      members = SeqMap(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "source"),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "port")
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "sink"),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "sink" + "port")
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper(DesignPath() + "link"),
          members = SeqMap(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "link" + "source")
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "link" + "sinks")
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_sink"),
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
      data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath()),
      members = SeqMap(
        "source" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "source"),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "port")
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "source" + "inner"),
              members = SeqMap(
                "port" -> EdgirGraph.EdgirPort(
                  data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "source" + "inner" + "port")
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "source" + "export_inner"),
              source = Seq("inner", "port"),
              target = Seq("port")
            )
          )
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "sink"),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "sink" + "port")
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = EdgirTestUtils.Dummy.BlockWrapper(DesignPath() + "sink" + "inner"),
              members = SeqMap(
                "port" -> EdgirGraph.EdgirPort(
                  data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "sink" + "inner" + "port")
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "sink" + "export_inner"),
              source = Seq("inner", "port"),
              target = Seq("port")
            )
          )
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = EdgirTestUtils.Dummy.LinkWrapper(DesignPath() + "link"),
          members = SeqMap(
            "source" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "link" + "source")
            ),
            "sinks" -> EdgirGraph.EdgirPort(
              data = EdgirTestUtils.Dummy.PortWrapper(DesignPath() + "link" + "sinks")
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_source"),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = EdgirTestUtils.Dummy.ConnectWrapper(DesignPath() + "connect_sink"),
          source = Seq("sink", "port"),
          target = Seq("link", "sinks")
        ),
      )
    )
  }
}
