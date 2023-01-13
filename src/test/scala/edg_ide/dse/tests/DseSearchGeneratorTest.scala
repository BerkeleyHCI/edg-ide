package edg_ide.dse.tests

import edgir.schema.schema
import edg.wir.{DesignPath, EdgirLibrary, Library, Refinements}
import edg.compiler.{Compiler, IntValue, PartialCompile, RangeValue, TextValue}
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
      Seq(10, 11).map(IntValue(_))
    )

    val partial12 = PartialCompile(params=Seq(
      DesignPath() + "param1",
      DesignPath() + "param2",
    ))
    val partial2 = PartialCompile(params = Seq(
      DesignPath() + "param2",
    ))

    val generator = new DseSearchGenerator(Seq(valuesConfig1, valuesConfig2))

    generator.nextPoint() should equal(Some(None, partial12, SeqMap(), Refinements()))
    val dummyBaseCompiler = new Compiler(schema.Design(), new EdgirLibrary(schema.Library()))
    generator.addEvaluatedPoint(dummyBaseCompiler)

    generator.nextPoint() should equal(Some(Some(dummyBaseCompiler), partial2,
      SeqMap(valuesConfig1 -> 0),
      Refinements(instanceValues=Map(DesignPath() + "param1" -> IntValue(0)))))
  }
}
