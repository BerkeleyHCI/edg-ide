package edg_ide.edgir_graph.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edg.elem.elem
import edg.expr.expr
import edg.wir.DesignPath
import edg_ide.edgir_graph.{BlockWrapper, ConnectWrapper, EdgirGraph, LinkWrapper, PortWrapper}
import edg_ide.edgir_graph.EdgirGraph.EdgirNode
import edg.ExprBuilder.ValueExpr

import scala.collection.SeqMap


class EdgirGraphTest extends AnyFlatSpec with Matchers {
  behavior of "EdgirGraph"

  it should "work on a flat source-to-sink design" in {
    val blockIr = elem.HierarchyBlock(
      blocks=Map(
        "source" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ),
        ))),
        "sink" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSink)
            ))),
          ),
        ))),
      ),
      links=Map(
        "link" -> elem.LinkLike(`type`=elem.LinkLike.Type.Link(elem.Link(
          selfClass=Some(EdgirTestUtils.Links.Power),
          ports=Map(
            "source" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
            "sink" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ),
        ))),
      ),
      constraints=Map(
        "connect_source" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(ValueExpr.Ref("source", "port")),
          linkPort = Some(ValueExpr.Ref("link", "source"))
        ))),
        "connect_sink" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(ValueExpr.Ref("sink", "port")),
          linkPort = Some(ValueExpr.Ref("link", "sink"))
        ))),
      ),
    )
    val blocklikeIr = elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      DesignPath(),
      blocklikeIr
    )

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(DesignPath(), blocklikeIr),
      members = SeqMap(
        "source" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "source", blockIr.blocks("source")),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "source" + "port",
                blockIr.blocks("source").`type`.hierarchy.get.ports("port"))
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "sink", blockIr.blocks("sink")),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "sink" + "port",
                blockIr.blocks("sink").`type`.hierarchy.get.ports("port"))
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = LinkWrapper(DesignPath() + "link", blockIr.links("link")),
          members = SeqMap(
            "source" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "link" + "source",
                blockIr.links("link").`type`.link.get.ports("source"))
            ),
            "sink" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "link" + "sink",
                blockIr.links("link").`type`.link.get.ports("sink"))
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = ConnectWrapper(DesignPath() + "connect_source", blockIr.constraints("connect_source")),
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = ConnectWrapper(DesignPath() + "connect_sink", blockIr.constraints("connect_sink")),
          source = Seq("sink", "port"),
          target = Seq("link", "sink")
        ),
      )
    )

    // These checks allow better error localization
    edgirGraph.data should equal(refGraph.data)
    edgirGraph.members("source") should equal(refGraph.members("source"))
    edgirGraph.members("sink") should equal(refGraph.members("sink"))
    edgirGraph.members("link") should equal(refGraph.members("link"))
    edgirGraph.edges should equal(refGraph.edges)

    // The final catch-all check
    edgirGraph should equal(refGraph)
  }

  it should "work on a simple hierarchy export design" in {
    val blockIr = elem.HierarchyBlock(
      blocks=Map(
        "outer" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ),
          blocks=Map(
            "inner" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
              ports=Map(
                "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                  selfClass=Some(EdgirTestUtils.Ports.PowerSource)
                ))),
              ),
            ))),
          ),
          constraints=Map(
            "export" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Exported(expr.ExportedExpr(
              internalBlockPort = Some(ValueExpr.Ref("inner", "port")),
              exteriorPort = Some(ValueExpr.Ref("port"))
            ))),
          ),
        ))),
      ),
      links=Map(),
      constraints=Map(),
    )
    val blocklikeIr = elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      DesignPath(),
      blocklikeIr
    )

    val outerBlockIr = blockIr.blocks("outer").`type`.hierarchy.get

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(DesignPath(), blocklikeIr),
      members = SeqMap(
        "outer" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "outer", blockIr.blocks("outer")),
          members = SeqMap(
            "port" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "outer" + "port", outerBlockIr.ports("port"))
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = BlockWrapper(DesignPath() + "outer" + "inner", outerBlockIr.blocks("inner")),
              members = SeqMap(
                "port" -> EdgirGraph.EdgirPort(
                  data = PortWrapper(DesignPath() + "outer" + "inner" + "port",
                    outerBlockIr.blocks("inner").`type`.hierarchy.get.ports("port"))
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = ConnectWrapper(DesignPath() + "outer" + "export", outerBlockIr.constraints("export")),
              source = Seq("inner", "port"),
              target = Seq("port")
            ),
          )
        ),
      ),
      edges = Seq(),
    )

    // These checks allow better error localization
    edgirGraph.members("outer").asInstanceOf[EdgirNode].edges should equal(
      refGraph.members("outer").asInstanceOf[EdgirNode].edges)
    edgirGraph.members("outer").asInstanceOf[EdgirNode].members("inner") should equal(
      refGraph.members("outer").asInstanceOf[EdgirNode].members("inner"))

    // The final catch-all check
    edgirGraph should equal(refGraph)
  }
}
