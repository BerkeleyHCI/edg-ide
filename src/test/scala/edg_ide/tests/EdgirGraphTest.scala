package edg_ide.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import edg_ide.{EdgirUtils, EdgirGraph, EdgirLibrary, BlockWrapper, LinkWrapper}
import edg.elem.elem
import edg.expr.expr
import edg.schema.schema


class EdgirGraphTest extends AnyFlatSpec with Matchers {
  // Some definitions that need to be kept consistent with the Python HDL / frontend
  object Ports {
    val PowerSource = EdgirUtils.StringToLibraryPath("electronics_model.ElectricalPorts.ElectricalSource")
    val PowerSink = EdgirUtils.StringToLibraryPath("electronics_model.ElectricalPorts.ElectricalSource")
  }
  object Links {
    val Power = EdgirUtils.StringToLibraryPath("electronics_model.ElectricalPorts.ElectricalLink")
  }

  def makeSimpleSourceSink(): elem.HierarchyBlock = {
    elem.HierarchyBlock(
      blocks=Map(
        "source" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(Ports.PowerSource)
            ))),
          ),
        ))),
        "sink" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports=Map(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(Ports.PowerSink)
            ))),
          ),
        ))),
      ),
      links=Map(
        "link" -> elem.LinkLike(`type`=elem.LinkLike.Type.Link(elem.Link(
          superclasses=Seq(Links.Power),
          ports=Map(
            "source" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(Ports.PowerSource)
            ))),
            "sink" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              superclasses=Seq(Ports.PowerSource)
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
  }

  behavior of "EdgirGraph"

  it should "work on a flat source-to-sink design" in {
    val blockIr = makeSimpleSourceSink()
    val blocklikeIr = elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      blocklikeIr,
      new EdgirLibrary(schema.Library())
    )

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(blocklikeIr),
      members = Map(
        "source" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(blockIr.blocks("source")),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = blockIr.blocks("source").`type`.hierarchy.get.ports("port")
            ),
          ),
          edges = Seq()
        ),
        "sink" -> EdgirGraph.EdgirNode(
          data = BlockWrapper(blockIr.blocks("sink")),
          members = Map(
            "port" -> EdgirGraph.EdgirPort(
              data = blockIr.blocks("sink").`type`.hierarchy.get.ports("port")
            ),
          ),
          edges = Seq()
        ),
        "link" -> EdgirGraph.EdgirNode(
          data = LinkWrapper(blockIr.links("link")),
          members = Map(
            "source" -> EdgirGraph.EdgirPort(
              data = blockIr.links("link").`type`.link.get.ports("source")
            ),
            "sink" -> EdgirGraph.EdgirPort(
              data = blockIr.links("link").`type`.link.get.ports("sink")
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
}
