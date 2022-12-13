package edg_ide.util.tests

import edg.ExprBuilder.ValueExpr
import edg.wir.ProtoUtil.{BlockSeqMapToProto, ConstraintSeqMapToProto, LinkSeqMapToProto, PortSeqMapToProto}
import edg_ide.edgir_graph.tests.EdgirTestUtils
import edg_ide.runner.PDFGeneratorUtil
import edgir.elem.elem
import edgir.expr.expr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{BufferedInputStream, FileInputStream}
import scala.collection.SeqMap
import scala.language.postfixOps


class PDFGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "PDFGeneratorUtil"

  it should "generate a working PDF file" in {

    val blockIr = elem.HierarchyBlock(
      blocks = SeqMap(
        "source" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports = SeqMap(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ).toPb,
          blocks = SeqMap(
            "inner_block" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
              ports = SeqMap(
                "inner_port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                  selfClass=Some(EdgirTestUtils.Ports.PowerSource),
                ))),
              ).toPb,
            ))),
          ).toPb,
        ))),
        "sink" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports = SeqMap(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSink)
            ))),
          ).toPb,
        ))),
      ).toPb,
      links = SeqMap(
        "link" -> elem.LinkLike(`type`=elem.LinkLike.Type.Link(elem.Link(
          selfClass=Some(EdgirTestUtils.Links.Power),
          ports = SeqMap(
            "source" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
            "sink" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ).toPb,
        ))),
      ).toPb,
      constraints = SeqMap(
        "connect_source" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(ValueExpr.Ref("source", "port")),
          linkPort = Some(ValueExpr.Ref("link", "source"))
        ))),
        "connect_sink" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(ValueExpr.Ref("sink", "port")),
          linkPort = Some(ValueExpr.Ref("link", "sink"))
        ))),
      ).toPb,
    )

    PDFGeneratorUtil.generate(blockIr, "unit_test.pdf")

    val bufferedInputStream = new BufferedInputStream(new FileInputStream("unit_test.pdf"))
    val byteArray = LazyList.continually(bufferedInputStream.read).takeWhile(-1 !=).map(_.toByte).toArray

    byteArray should not be empty
  }
}
