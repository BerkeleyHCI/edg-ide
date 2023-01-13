package edg_ide.dse.tests

import edg.compiler.{BooleanValue, ExprValue, FloatValue, IntValue, RangeValue, TextValue}
import edg.wir.DesignPath
import edg_ide.dse.{DseParameterSearch, DseSearchGenerator}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DseSearchGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "DseSearchGenerator"

  it should "parse String to BooleanValue" in {
    val valuesConfig1 = DseParameterSearch(DesignPath() + "param1",
      Seq(0, 1, 2).map(IntValue(_))
    )
    val valuesConfig2 = DseParameterSearch(DesignPath() + "param1",
      Seq(10, 11, 12).map(IntValue(_))
    )

    val generator = new DseSearchGenerator(Seq(valuesConfig1, valuesConfig2))

  }
}
