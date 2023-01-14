package edg_ide.dse.tests

import edg.compiler.{Compiler, IntValue, PartialCompile}
import edg.wir.{DesignPath, EdgirLibrary, Refinements}
import edg_ide.dse.{DseDerivedPartSearch, DseParameterSearch, DseSearchGenerator}
import edgir.schema.schema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap


class MockCompiler extends Compiler(schema.Design(), new EdgirLibrary(schema.Library())) {

}


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

    val rootCompiler = new MockCompiler()
    generator.addEvaluatedPoint(rootCompiler)
    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(valuesConfig1 -> IntValue(0)),
      Refinements(instanceValues=Map(DesignPath() + "param1" -> IntValue(0)))))

    val fork0Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork0Compiler)
    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(0), valuesConfig2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10)))))

    generator.addEvaluatedPoint(fork0Compiler)
    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(0), valuesConfig2 -> IntValue(11)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(11)))))

    generator.addEvaluatedPoint(fork0Compiler)
    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(0), valuesConfig2 -> IntValue(12)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(12)))))

    // Param 2 searched, backtrack to param 1
    generator.addEvaluatedPoint(rootCompiler)
    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(valuesConfig1 -> IntValue(1)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(1)))))

    val fork1Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork1Compiler)
    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(1), valuesConfig2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10)))))

    generator.addEvaluatedPoint(fork1Compiler)
    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(1), valuesConfig2 -> IntValue(11)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(11)))))

    generator.addEvaluatedPoint(fork1Compiler)
    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(valuesConfig1 -> IntValue(1), valuesConfig2 -> IntValue(12)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(12)))))

    generator.addEvaluatedPoint(fork1Compiler)
    generator.nextPoint() should equal(None)
  }

  it should "generate a derived config space" in {
    val partConfig = DseDerivedPartSearch(DesignPath())

    val partial = PartialCompile(params = Seq(
      DesignPath() + "part"
    ))
    val partialEmpty = PartialCompile()

    val generator = new DseSearchGenerator(Seq(partConfig))
    generator.nextPoint() should equal(Some(None, partial,  // first is the base compile holding back everything
      SeqMap(),
      Refinements(instanceValues = Map())))

    val rootCompiler = new MockCompiler()
    generator.addEvaluatedPoint(rootCompiler)
    generator.nextPoint() should equal(Some(None, partial, SeqMap(), Refinements()))  // generating compile is next

    // TODO scan parts space
  }
}