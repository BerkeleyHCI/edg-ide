package edg_ide.edgir_graph.tests

import edg.ExprBuilder.ValueExpr
import edg_ide.runner.PDFGeneratorUtil
import edgir.elem.elem
import edgir.expr.expr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{BufferedInputStream, FileInputStream}
import scala.language.postfixOps


class PDFGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "PDFGeneratorUtil"

  it should "generate a working PDF file" in {

    val blockIr = elem.HierarchyBlock(
      blocks=Map(
        "source" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
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

    PDFGeneratorUtil.generate( blockIr, "unit_test.pdf")

    val bufferedInputStream = new BufferedInputStream(new FileInputStream("unit_test.pdf"))
    val byteArray = LazyList.continually(bufferedInputStream.read).takeWhile(-1 !=).map(_.toByte).toArray

    byteArray should not be empty
  }
}
