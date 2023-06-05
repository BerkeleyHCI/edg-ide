package edg_ide.util.tests

import edg.ElemBuilder.LibraryPath
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
        "R1" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports = SeqMap(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass = Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ).toPb,
          blocks = SeqMap(
            "inner_source_block" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
              ports = SeqMap(
                "inner_source_port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                  selfClass = Some(EdgirTestUtils.Ports.PowerSource),
                ))),
              ).toPb,
              selfClass = Some(LibraryPath("JlcResistor")),
            ))),
          ).toPb,
          selfClass = Some(LibraryPath("electronics_lib.GenericResistor.GenericAxialResistor")),
        ))),
        "R2" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
          ports = SeqMap(
            "port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSink)
            ))),
          ).toPb,
          blocks = SeqMap(
            "inner_sink_block" -> elem.BlockLike(`type`=elem.BlockLike.Type.Hierarchy(elem.HierarchyBlock(
              ports = SeqMap(
                "inner_sink_port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
                  selfClass=Some(EdgirTestUtils.Ports.PowerSource),
                ))),
              ).toPb,
              selfClass = Some(LibraryPath("JlcResistor")),
            ))),
          ).toPb,
          selfClass = Some(LibraryPath("electronics_lib.GenericResistor.GenericAxialResistor")),
        ))),
      ).toPb,
      links = SeqMap(
        "link" -> elem.LinkLike(`type`=elem.LinkLike.Type.Link(elem.Link(
          selfClass=Some(EdgirTestUtils.Links.Power),
          ports = SeqMap(
            "R1_port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
            "R2_port" -> elem.PortLike(is=elem.PortLike.Is.Port(elem.Port(
              selfClass=Some(EdgirTestUtils.Ports.PowerSource)
            ))),
          ).toPb,
        ))),
      ).toPb,
      constraints = SeqMap(
        "connect_R1" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(ValueExpr.Ref("R1", "port")),
          linkPort = Some(ValueExpr.Ref("link", "R1_port"))
        ))),
        "connect_R2" -> expr.ValueExpr(expr=expr.ValueExpr.Expr.Connected(expr.ConnectedExpr(
          blockPort = Some(ValueExpr.Ref("R2", "port")),
          linkPort = Some(ValueExpr.Ref("link", "R2_port"))
        ))),
      ).toPb,
      selfClass = Some(LibraryPath("ResistorChain")),
    )

    PDFGeneratorUtil.generate(blockIr, fileName="unit_test.pdf")

    val bufferedInputStream = new BufferedInputStream(new FileInputStream("unit_test.pdf"))
    val byteArray = LazyList.continually(bufferedInputStream.read).takeWhile(-1 !=).map(_.toByte).toArray

    byteArray should not be empty
  }
}
