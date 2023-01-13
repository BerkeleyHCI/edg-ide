package edg_ide.dse.tests

import edg.compiler.{Compiler, IntValue, PartialCompile}
import edg.wir.{DesignPath, EdgirLibrary, Refinements}
import edg_ide.dse.{DseParameterSearch, DseSearchGenerator}
import edgir.schema.schema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap


class DseSearchGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "DseSearchGenerator"

  it should "generate a static config space" in {
    val valuesConfig1 = DseParameterSearch(DesignPath() + "param1",
      Seq(0, 1).map(IntValue(_))
    )
    val valuesConfig2 = DseParameterSearch(DesignPath() + "param2",
      Seq(10, 11, 12).map(IntValue(_))
    )

    val partial12 = PartialCompile(params=Seq(
      DesignPath() + "param1",
      DesignPath() + "param2",
    ))
    val partial2 = PartialCompile(params = Seq(
      DesignPath() + "param2",
    ))
    val partialEmpty = PartialCompile()

    val generator = new DseSearchGenerator(Seq(valuesConfig1, valuesConfig2))
    generator.nextPoint() should equal(Some(None, partial12, SeqMap(), Refinements()))

    val rootCompiler = new Compiler(schema.Design(), new EdgirLibrary(schema.Library()))
    generator.addEvaluatedPoint(rootCompiler)
    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(valuesConfig1 -> IntValue(0)),
      Refinements(instanceValues=Map(DesignPath() + "param1" -> IntValue(0)))))

    val fork0Compiler = rootCompiler.fork()
    generator.addEvaluatedPoint(fork0Compiler)
    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(0), valuesConfig2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(0), DesignPath() + "param2" -> IntValue(10)))))

    generator.addEvaluatedPoint(fork0Compiler)
    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(0), valuesConfig2 -> IntValue(11)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(0), DesignPath() + "param2" -> IntValue(11)))))
  }
}
