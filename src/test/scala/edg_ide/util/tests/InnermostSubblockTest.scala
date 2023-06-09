package edg_ide.util.tests

import edg.ElemBuilder.{Block, Design}
import edg.wir.DesignPath
import edg_ide.util.EdgirAnalysisUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap

class InnermostSubblockTest extends AnyFlatSpec with Matchers {
  behavior.of("getInnermostSubblock")

  it should "get innermost subblocks" in {
    val design = Design(Block.Block(
      selfClass = "top",
      blocks = SeqMap(
        "inner" -> Block.Block("innerBlock"),
      )
    ))

    EdgirAnalysisUtils.getInnermostSubblock(DesignPath(), design.getContents) shouldEqual
      Some((DesignPath() + "inner", design.getContents.blocks(0).getValue.getHierarchy))
  }

  it should "get nested innermost subblocks" in {
    val design = Design(Block.Block(
      selfClass = "top",
      blocks = SeqMap(
        "outer" -> Block.Block(
          "outerBlock",
          blocks = SeqMap(
            "inner" -> Block.Block("innerBlock"),
          ),
        ),
      )
    ))

    EdgirAnalysisUtils.getInnermostSubblock(DesignPath(), design.getContents) shouldEqual
      Some((
        DesignPath() + "outer" + "inner",
        design.getContents.blocks(0).getValue.getHierarchy.blocks(0).getValue.getHierarchy
      ))
  }

  it should "ignore bridges" in {
    val design = Design(Block.Block(
      selfClass = "top",
      blocks = SeqMap(
        "(bridge)bridge" -> Block.Block("bridge"),
        "inner" -> Block.Block("innerBlock"),
      )
    ))

    EdgirAnalysisUtils.getInnermostSubblock(DesignPath(), design.getContents) shouldEqual
      Some((DesignPath() + "inner", design.getContents.blocks(1).getValue.getHierarchy))
  }

  it should "return self" in {
    val design = Design(Block.Block(
      selfClass = "top",
      blocks = SeqMap(
      )
    ))

    EdgirAnalysisUtils.getInnermostSubblock(DesignPath(), design.getContents) shouldEqual
      Some((DesignPath(), design.getContents))
  }

  it should "ignore multiple subblocks" in {
    val design = Design(Block.Block(
      selfClass = "top",
      blocks = SeqMap(
        "inner1" -> Block.Block("innerBlock"),
        "inner2" -> Block.Block("innerBlock"),
      )
    ))

    EdgirAnalysisUtils.getInnermostSubblock(DesignPath(), design.getContents) shouldEqual None
  }
}
