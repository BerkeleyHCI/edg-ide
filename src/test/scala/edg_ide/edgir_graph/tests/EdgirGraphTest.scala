package edg_ide.edgir_graph.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edg_ide.{EdgirLibrary, EdgirUtils}
import edg.elem.elem
import edg.expr.expr
import edg.schema.schema
import edg_ide.edgir_graph.{EdgirGraph, BlockWrapper, LinkWrapper, PortWrapper}
import edg_ide.edgir_graph.EdgirGraph.EdgirNode


class EdgirGraphTest extends AnyFlatSpec with Matchers {
  behavior of "EdgirGraph"

  it should "work on a flat source-to-sink design" in {
    val blockIr = elem.HierarchyBlock(
      blocks=Map(
        "source" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(EdgirTestUtils.Ports.PowerSource)
            ))),
          ),
        ))),
        "sink" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(EdgirTestUtils.Ports.PowerSink)
            ))),
          ),
        ))),
      ),
      links=Map(
        "link" -> elem.LinkLike(`type`=elem.LinkLike.Type.Link(elem.Link(
          superclasses=Seq(EdgirTestUtils.Links.Power),
          ports=Map(
            "source" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(EdgirTestUtils.Ports.PowerSource)
            ))),
            "sink" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(EdgirTestUtils.Ports.PowerSource)
            ))),
          ),
        ))),
      ),
      constraints=Map(
        "connect_source" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(EdgirUtils.SeqStringToRefExpr(Seq("source", "port"))),
          linkPort = Some(EdgirUtils.SeqStringToRefExpr(Seq("link", "source")))
        ))),
        "connect_sink" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(EdgirUtils.SeqStringToRefExpr(Seq("sink", "port"))),
          linkPort = Some(EdgirUtils.SeqStringToRefExpr(Seq("link", "sink")))
        ))),
      ),
    )
    val blocklikeIr = elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      blocklikeIr,
      "root",
      new EdgirLibrary(schema.Library())
    )

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(blocklikeIr),
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(blockIr.blocks("source")),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = PortWrapper(blockIr.blocks("source").`type`.hierarchy.get.ports("port"))
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(blockIr.blocks("sink")),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = PortWrapper(blockIr.blocks("sink").`type`.hierarchy.get.ports("port"))
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = LinkWrapper(blockIr.links("link")),
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = PortWrapper(blockIr.links("link").`type`.link.get.ports("source"))
            ),
            "sink" -> EdgirGraph.EdgirPort(
              data = PortWrapper(blockIr.links("link").`type`.link.get.ports("sink"))
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = "connect_source",
          source = Seq("source", "port"),
          target = Seq("link", "source")
        ),
        EdgirGraph.EdgirEdge(
          data = "connect_sink",
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
              superclasses=Seq(EdgirTestUtils.Ports.PowerSource)
            ))),
          ),
          blocks=Map(
            "inner" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
              ports=Map(
                "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                  superclasses=Seq(EdgirTestUtils.Ports.PowerSource)
                ))),
              ),
            ))),
          ),
          constraints=Map(
            "export" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Exported(expr.ExportedExpr(
              internalBlockPort = Some(EdgirUtils.SeqStringToRefExpr(Seq("inner", "port"))),
              exteriorPort = Some(EdgirUtils.SeqStringToRefExpr(Seq("port")))
            ))),
          ),
        ))),
      ),
      links=Map(),
      constraints=Map(),
    )
    val blocklikeIr = elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      blocklikeIr,
      "root",
      new EdgirLibrary(schema.Library())
    )

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(blocklikeIr),
      members = Map(
        "outer" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(blockIr.blocks("outer")),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = PortWrapper(blockIr.blocks("outer").`type`.hierarchy.get.ports("port"))
            ),
            "inner" -> EdgirGraph.EdgirNode(
              data = BlockWrapper(blockIr.blocks("outer").`type`.hierarchy.get.blocks("inner")),
              members = Map(
                "port" -> EdgirGraph.EdgirPort(
                  data = PortWrapper(blockIr.blocks("outer").`type`.hierarchy.get
                      .blocks("inner").`type`.hierarchy.get.ports("port"))
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = "export",
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
