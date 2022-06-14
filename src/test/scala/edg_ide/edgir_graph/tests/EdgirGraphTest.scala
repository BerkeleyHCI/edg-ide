package edg_ide.edgir_graph.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edgir.elem.elem
import edgir.expr.expr
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

  it should "work with block-side arrays" in {
    val designIr = elem.HierarchyBlock(
      blocks=Map(
        "block" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "ports" -> elem.PortLike(is=elem.PortLike.Is.Array(elem.PortArray(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource),
              contains=elem.PortArray.Contains.Ports(elem.PortArray.Ports(
                ports=Map(
                  "0" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                    selfClass=Some(EdgirTestUtils.Ports.PowerSource)
                  ))),
                  "test" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                    selfClass=Some(EdgirTestUtils.Ports.PowerSource)
                  ))),
                )
              ))
            ))),
          ),
        ))),
      ),
      links=Map(),
      constraints=Map(),
    )
    val designBlocklikeIr = elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(designIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      DesignPath(),
      designBlocklikeIr
    )

    val blockIr = designIr.blocks("block").`type`.hierarchy.get

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(DesignPath(), designBlocklikeIr),
      members = SeqMap(
        "block" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "block", designIr.blocks("block")),
          members = SeqMap(
            "ports[...]" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "block" + "ports",
                blockIr.ports("ports"))
            ),
            "ports[0]" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "block" + "ports" + "0",
                blockIr.ports("ports").getArray.getPorts.ports("0"))
            ),
            "ports[test]" -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "block" + "ports" + "test",
                blockIr.ports("ports").getArray.getPorts.ports("test"))
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(),
    )

    // These checks allow better error localization
    edgirGraph.members("block").asInstanceOf[EdgirNode].members("ports[...]") should equal(
      refGraph.members("block").asInstanceOf[EdgirNode].members("ports[...]"))
    edgirGraph.members("block").asInstanceOf[EdgirNode].members("ports[0]") should equal(
      refGraph.members("block").asInstanceOf[EdgirNode].members("ports[0]"))
    edgirGraph.members("block").asInstanceOf[EdgirNode].members("ports[test]") should equal(
      refGraph.members("block").asInstanceOf[EdgirNode].members("ports[test]"))

    // The final catch-all check
    edgirGraph should equal(refGraph)
  }
}
