package edg_ide.dse.tests

import edg.compiler.{BooleanValue, ExprValue, FloatValue, IntValue, PartialCompile, RangeValue, TextValue}
import edg.wir.{DesignPath, Refinements}
import edg_ide.dse.{DseParameterSearch, DseSearchGenerator}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap


class DseSearchGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "DseSearchGenerator"

  it should "generate a static config space" in {
    val valuesConfig1 = DseParameterSearch(DesignPath() + "param1",
      Seq(0, 1, 2).map(IntValue(_))
    )
    val valuesConfig2 = DseParameterSearch(DesignPath() + "param2",
      Seq(10, 11, 12).map(IntValue(_))
    )

    val partial12 = PartialCompile(params=Seq(
      DesignPath() + "param1",
      DesignPath() + "param2",
    ))

    val generator = new DseSearchGenerator(Seq(valuesConfig1, valuesConfig2))

    generator.nextPoint() should equal(Some(None, partial12, SeqMap(), Refinements()))
  }
}
