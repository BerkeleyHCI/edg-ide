package edg_ide.edgir_graph.tests

import edg.ExprBuilder.ValueExpr
import edg.wir.DesignPath
import edg.wir.ProtoUtil._
import edg_ide.edgir_graph.EdgirGraph.EdgirNode
import edg_ide.edgir_graph._
import edgir.elem.elem
import edgir.expr.expr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap

class EdgirGraphTest extends AnyFlatSpec with Matchers {
  behavior.of("EdgirGraph")

  it should "work on a flat source-to-sink design" in {
    val blockIr = elem.HierarchyBlock(
      blocks = SeqMap(
        "source" -> elem.BlockLike(`type` =
          elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
            ports = SeqMap(
              "port" -> elem.PortLike(is =
                elem.PortLike.Is.Port(elem.Port(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                ))
              ),
            ).toPb,
          ))
        ),
        "sink" -> elem.BlockLike(`type` =
          elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
            ports = SeqMap(
              "port" -> elem.PortLike(is =
                elem.PortLike.Is.Port(elem.Port(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSink)
                ))
              ),
            ).toPb,
          ))
        ),
      ).toPb,
      links = SeqMap(
        "link" -> elem.LinkLike(`type` =
          elem.LinkLike.Type.Link(elem.Link(
            selfClass = Some(EdgirTestUtils.Links.Power),
            ports = SeqMap(
              "source" -> elem.PortLike(is =
                elem.PortLike.Is.Port(elem.Port(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                ))
              ),
              "sink" -> elem.PortLike(is =
                elem.PortLike.Is.Port(elem.Port(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                ))
              ),
            ).toPb,
          ))
        ),
      ).toPb,
      constraints = SeqMap(
        "connect_source" -> expr.ValueExpr(expr =
          expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
            blockPort = Some(ValueExpr.Ref("source", "port")),
            linkPort = Some(ValueExpr.Ref("link", "source"))
          ))
        ),
        "connect_sink" -> expr.ValueExpr(expr =
          expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
            blockPort = Some(ValueExpr.Ref("sink", "port")),
            linkPort = Some(ValueExpr.Ref("link", "sink"))
          ))
        ),
      ).toPb,
    )
    val blocklikeIr = elem.BlockLike(`type` = elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      DesignPath(),
      blocklikeIr
    )

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(DesignPath(), blocklikeIr),
      members = SeqMap(
        Seq("source") -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "source", blockIr.blocks("source")),
          members = SeqMap(
            Seq("port") -> EdgirGraph.EdgirPort(
              data = PortWrapper(
                DesignPath() + "source" + "port",
                blockIr.blocks("source").`type`.hierarchy.get.ports("port")
              )
            ),
          ),
          edges = Seq()
        ),
        Seq("sink") -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "sink", blockIr.blocks("sink")),
          members = SeqMap(
            Seq("port") -> EdgirGraph.EdgirPort(
              data =
                PortWrapper(DesignPath() + "sink" + "port", blockIr.blocks("sink").`type`.hierarchy.get.ports("port"))
            ),
          ),
          edges = Seq()
        ),
        Seq("link") -> EdgirGraph.EdgirNode(
          data = LinkWrapper(DesignPath() + "link", blockIr.links("link")),
          members = SeqMap(
            Seq("source") -> EdgirGraph.EdgirPort(
              data =
                PortWrapper(DesignPath() + "link" + "source", blockIr.links("link").`type`.link.get.ports("source"))
            ),
            Seq("sink") -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "link" + "sink", blockIr.links("link").`type`.link.get.ports("sink"))
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(
        EdgirGraph.EdgirEdge(
          data = ConnectWrapper(DesignPath() + "connect_source", blockIr.constraints("connect_source")),
          source = Some(Seq("source", "port")),
          target = Some(Seq("link", "source"))
        ),
        EdgirGraph.EdgirEdge(
          data = ConnectWrapper(DesignPath() + "connect_sink", blockIr.constraints("connect_sink")),
          source = Some(Seq("sink", "port")),
          target = Some(Seq("link", "sink"))
        ),
      )
    )

    // These checks allow better error localization
    edgirGraph.data should equal(refGraph.data)
    edgirGraph.members(Seq("source")) should equal(refGraph.members(Seq("source")))
    edgirGraph.members(Seq("sink")) should equal(refGraph.members(Seq("sink")))
    edgirGraph.members(Seq("link")) should equal(refGraph.members(Seq("link")))
    edgirGraph.edges should equal(refGraph.edges)

    // The final catch-all check
    edgirGraph should equal(refGraph)
  }

  it should "work on a simple hierarchy export design" in {
    val blockIr = elem.HierarchyBlock(
      blocks = SeqMap(
        "outer" -> elem.BlockLike(`type` =
          elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
            ports = SeqMap(
              "port" -> elem.PortLike(is =
                elem.PortLike.Is.Port(elem.Port(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                ))
              ),
            ).toPb,
            blocks = SeqMap(
              "inner" -> elem.BlockLike(`type` =
                elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
                  ports = SeqMap(
                    "port" -> elem.PortLike(is =
                      elem.PortLike.Is.Port(elem.Port(
                        selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                      ))
                    ),
                  ).toPb,
                ))
              ),
            ).toPb,
            constraints = SeqMap(
              "export" -> expr.ValueExpr(expr =
                expr.ValueExpr.Expr.Exported(expr.ExportedExpr(
                  internalBlockPort = Some(ValueExpr.Ref("inner", "port")),
                  exteriorPort = Some(ValueExpr.Ref("port"))
                ))
              ),
            ).toPb,
          ))
        ),
      ).toPb,
      links = Seq(),
      constraints = Seq(),
    )
    val blocklikeIr = elem.BlockLike(`type` = elem.BlockLike.Type.Hierarchy(blockIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      DesignPath(),
      blocklikeIr
    )

    val outerBlockIr = blockIr.blocks("outer").`type`.hierarchy.get

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(DesignPath(), blocklikeIr),
      members = SeqMap(
        Seq("outer") -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "outer", blockIr.blocks("outer")),
          members = SeqMap(
            Seq("port") -> EdgirGraph.EdgirPort(
              data = PortWrapper(DesignPath() + "outer" + "port", outerBlockIr.ports("port"))
            ),
            Seq("inner") -> EdgirGraph.EdgirNode(
              data = BlockWrapper(DesignPath() + "outer" + "inner", outerBlockIr.blocks("inner")),
              members = SeqMap(
                Seq("port") -> EdgirGraph.EdgirPort(
                  data = PortWrapper(
                    DesignPath() + "outer" + "inner" + "port",
                    outerBlockIr.blocks("inner").`type`.hierarchy.get.ports("port")
                  )
                ),
              ),
              edges = Seq()
            ),
          ),
          edges = Seq(
            EdgirGraph.EdgirEdge(
              data = ConnectWrapper(DesignPath() + "outer" + "export", outerBlockIr.constraints("export")),
              source = Some(Seq("inner", "port")),
              target = Some(Seq("port"))
            ),
          )
        ),
      ),
      edges = Seq(),
    )

    // These checks allow better error localization
    edgirGraph.members(Seq("outer")).asInstanceOf[EdgirNode].edges should equal(
      refGraph.members(Seq("outer")).asInstanceOf[EdgirNode].edges
    )
    edgirGraph.members(Seq("outer")).asInstanceOf[EdgirNode].members(Seq("inner")) should equal(
      refGraph.members(Seq("outer")).asInstanceOf[EdgirNode].members(Seq("inner"))
    )

    // The final catch-all check
    edgirGraph should equal(refGraph)
  }

  it should "work with block-side arrays" in {
    val designIr = elem.HierarchyBlock(
      blocks = SeqMap(
        "block" -> elem.BlockLike(`type` =
          elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
            ports = SeqMap(
              "ports" -> elem.PortLike(is =
                elem.PortLike.Is.Array(elem.PortArray(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSource),
                  contains = elem.PortArray.Contains.Ports(elem.PortArray.Ports(
                    ports = SeqMap(
                      "0" -> elem.PortLike(is =
                        elem.PortLike.Is.Port(elem.Port(
                          selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                        ))
                      ),
                      "test" -> elem.PortLike(is =
                        elem.PortLike.Is.Port(elem.Port(
                          selfClass = Some(EdgirTestUtils.Ports.PowerSource)
                        ))
                      ),
                    ).toPb
                  ))
                ))
              ),
            ).toPb,
          ))
        ),
      ).toPb,
      links = Seq(),
      constraints = Seq(),
    )
    val designBlocklikeIr = elem.BlockLike(`type` = elem.BlockLike.Type.Hierarchy(designIr))
    val edgirGraph = EdgirGraph.blockLikeToNode(
      DesignPath(),
      designBlocklikeIr
    )

    val blockIr = designIr.blocks("block").`type`.hierarchy.get

    val refGraph = EdgirGraph.EdgirNode(
      data = BlockWrapper(DesignPath(), designBlocklikeIr),
      members = SeqMap(
        Seq("block") -> EdgirGraph.EdgirNode(
          data = BlockWrapper(DesignPath() + "block", designIr.blocks("block")),
          members = SeqMap(
            Seq("ports", "0") -> EdgirGraph.EdgirPort(
              data = PortWrapper(
                DesignPath() + "block" + "ports" + "0",
                blockIr.ports("ports").getArray.getPorts.ports.head.getValue
              )
            ),
            Seq("ports", "test") -> EdgirGraph.EdgirPort(
              data = PortWrapper(
                DesignPath() + "block" + "ports" + "test",
                blockIr.ports("ports").getArray.getPorts.ports.last.getValue
              )
            ),
          ),
          edges = Seq()
        ),
      ),
      edges = Seq(),
    )

    // These checks allow better error localization
    edgirGraph.members(Seq("block")).asInstanceOf[EdgirNode].members(Seq("ports", "0")) should equal(
      refGraph.members(Seq("block")).asInstanceOf[EdgirNode].members(Seq("ports", "0"))
    )

    // The final catch-all check
    edgirGraph should equal(refGraph)
  }
}
