package edg_ide.util.tests

import edg.ElemBuilder.{Block, Port, Design, Constraint}
import edg.ExprBuilder.{Ref, ValueExpr}
import edg.wir.DesignPath
import edg_ide.util.DesignFindDisconnected
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DesignFindDisconnectedTest extends AnyFlatSpec with Matchers {
  behavior of "DesignDisconnectedType"

  val testBlock = Block.Block(
    blocks = Map(
      "requiredConnectedBlock" -> Block.Block(
        ports = Map("port" -> Port.Port()),
        constraints = Map(
          "required" -> ValueExpr.Ref(Ref.IsConnected(Ref("port")))
        )
      ),
      "requiredDisconnectedBlock" -> Block.Block(
        ports = Map("port" -> Port.Port()),
        constraints = Map(
          "required" -> ValueExpr.Ref(Ref.IsConnected(Ref("port")))
        )
      ),
      "nonRequiredConnectedBlock" -> Block.Block(
        ports = Map("port" -> Port.Port())
      ),
      "nonRequiredDisconnectedBlock" -> Block.Block(
        ports = Map("port" -> Port.Port())
      ),
    ),
    constraints = Map(
      "requiredConnected" -> Constraint.Connected(Ref("requiredConnectedBlock", "port"), Ref("link")),
      "nonRequiredConnected" -> Constraint.Connected(Ref("nonRequiredConnectedBlock", "port"), Ref("link"))
    )
  )

  it should "find disconnected in the design top" in {
    val design = Design(testBlock)
    DesignFindDisconnected.map(design) should equal(
      (Seq(DesignPath() + "requiredDisconnectedBlock" + "port"), Seq()))
  }

  it should "find disconnected in a nested block" in {
    val design = Design(Block.Block(
      blocks = Map(
        "inner" -> testBlock
      )
    ))
    DesignFindDisconnected.map(design) should equal(
      (Seq(DesignPath() + "inner" + "requiredDisconnectedBlock" + "port"), Seq()))
  }
}
