package edg_ide.dse.tests

import edg.compiler.{Compiler, IntValue, PartialCompile}
import edg.util.Errorable
import edg.wir.{DesignPath, EdgirLibrary, Refinements}
import edg_ide.dse.{DseDerivedConfig, DseParameterSearch, DseSearchGenerator}
import edgir.schema.schema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SeqMap


class MockCompiler extends Compiler(schema.Design(), new EdgirLibrary(schema.Library())) {
}


case class DseDerivedStatic(path: DesignPath, var value: DseParameterSearch) extends DseDerivedConfig with Serializable {
  def configToString: String = f"DerivedStatic($path)"

  override def getPartialCompile: PartialCompile = {
    PartialCompile(params=Seq(path))
  }

  override def configFromDesign(compiledDesign: Compiler): Errorable[DseParameterSearch] = {
    Errorable.Success(value)
  }
}


class DseSearchGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "DseSearchGenerator"

  it should "generate a static config space" in {
    val config1 = DseParameterSearch(DesignPath() + "param1",
      Seq(0, 1).map(IntValue(_))
    )
    val config2 = DseParameterSearch(DesignPath() + "param2",
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

    val generator = new DseSearchGenerator(Seq(config1, config2))
    generator.nextPoint() should equal(Some(None, partial12, SeqMap(), Refinements(), 0.0))
    val rootCompiler = new MockCompiler()
    generator.addEvaluatedPoint(rootCompiler)

    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(config1 -> IntValue(0)),
      Refinements(instanceValues=Map(DesignPath() + "param1" -> IntValue(0))),
      0.0))
    val fork0Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork0Compiler)

    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(0), config2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10))),
      0.0))
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(0), config2 -> IntValue(11)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(11))),
      1.0f/6))
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(0), config2 -> IntValue(12)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(12))),
      2.0f/6))
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    // Param 2 searched, backtrack to param 1
    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(config1 -> IntValue(1)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(1))),
      3.0f/6))
    val fork1Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork1Compiler)

    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(1), config2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10))),
      3.0f/6))
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(1), config2 -> IntValue(11)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(11))),
      4.0f/6))
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(1), config2 -> IntValue(12)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(12))),
      5.0f/6))
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    generator.nextPoint() should equal(None)
  }

  it should "generate a derived config space" in {
    // we can't test dynamic behavior here since the derived space is generated only once at the test start
    val containedConfig1 = DseParameterSearch(DesignPath() + "param1",
      Seq(0, 1).map(IntValue(_))
    )
    val derivedConfig1 = DseDerivedStatic(DesignPath() + "param1", containedConfig1)
    val containedConfig2 = DseParameterSearch(DesignPath() + "param2",
      Seq(10).map(IntValue(_))
    )
    val derivedConfig2 = DseDerivedStatic(DesignPath() + "param2", containedConfig2)

    val partial12 = PartialCompile(params = Seq(DesignPath() + "param1", DesignPath() + "param2"))
    val partial2 = PartialCompile(params = Seq(DesignPath() + "param2"))
    val partialEmpty = PartialCompile()

    val generator = new DseSearchGenerator(Seq(derivedConfig1, derivedConfig2))
    generator.nextPoint() should equal(Some(None, partialEmpty,  // first is the generating compile
      SeqMap(),
      Refinements()))
    val generatingCompiler = new MockCompiler()
    generator.addEvaluatedPoint(generatingCompiler)  // should never be used

    generator.nextPoint() should equal(Some(None, partial12, SeqMap(), Refinements()))  // now the root compile
    val rootCompiler = new MockCompiler()
    generator.addEvaluatedPoint(rootCompiler)

    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(derivedConfig1 -> IntValue(0)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(0))))) // intermediate
    val fork0Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork0Compiler)

    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(derivedConfig1 -> IntValue(0), derivedConfig2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10))))) // concrete value
    generator.addEvaluatedPoint(rootCompiler)  // dummy - ignore

    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(derivedConfig1 -> IntValue(1)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(1))))) // intermediate
    val fork1Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork1Compiler)

    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(derivedConfig1 -> IntValue(1), derivedConfig2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10))))) // concrete value
    generator.addEvaluatedPoint(rootCompiler) // dummy - ignore

    generator.nextPoint() should equal(None)
  }

  it should "generate a hybrid derived config space" in {
    val config1 = DseParameterSearch(DesignPath() + "param1",
      Seq(0, 1).map(IntValue(_))
    )
    val containedConfig2 = DseParameterSearch(DesignPath() + "param2",
      Seq(10).map(IntValue(_))
    )
    val derivedConfig2 = DseDerivedStatic(DesignPath() + "param2", containedConfig2)

    val partial12 = PartialCompile(params = Seq(DesignPath() + "param1", DesignPath() + "param2"))
    val partial2 = PartialCompile(params = Seq(DesignPath() + "param2"))
    val partialEmpty = PartialCompile()

    val generator = new DseSearchGenerator(Seq(config1, derivedConfig2))
    generator.nextPoint() should equal(Some(None, partial12, // first is root compile pre-config1
      SeqMap(),
      Refinements(),
      0.0))
    val rootCompiler = new MockCompiler()
    generator.addEvaluatedPoint(rootCompiler)

    generator.nextPoint() should equal(Some(Some(rootCompiler), partialEmpty, // generating compile
      SeqMap(config1 -> IntValue(0)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(0))),
      0.0))
    val generatingCompiler1 = new MockCompiler()
    generator.addEvaluatedPoint(generatingCompiler1)  // should never be used

    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(config1 -> IntValue(0)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(0))),
      0.0)) // intermediate
    val fork0Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork0Compiler)

    generator.nextPoint() should equal(Some(Some(fork0Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(0), derivedConfig2 -> IntValue(10)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(10))),
      0.0)) // concrete value
    generator.addEvaluatedPoint(rootCompiler) // dummy - ignore

    // create a new dynamic value which should take effect
    val containedConfig2new = DseParameterSearch(DesignPath() + "param2",
      Seq(11).map(IntValue(_))
    )
    derivedConfig2.value = containedConfig2new

    generator.nextPoint() should equal(Some(Some(rootCompiler), partialEmpty, // generating compile
      SeqMap(config1 -> IntValue(1)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(1))),
      0.5))
    val generatingCompiler2 = new MockCompiler()
    generator.addEvaluatedPoint(generatingCompiler2) // should never be used

    generator.nextPoint() should equal(Some(Some(rootCompiler), partial2,
      SeqMap(config1 -> IntValue(1)),
      Refinements(instanceValues = Map(DesignPath() + "param1" -> IntValue(1))),
      0.5)) // intermediate
    val fork1Compiler = new MockCompiler()
    generator.addEvaluatedPoint(fork1Compiler)

    generator.nextPoint() should equal(Some(Some(fork1Compiler), partialEmpty,
      SeqMap(config1 -> IntValue(1), derivedConfig2 -> IntValue(11)),
      Refinements(instanceValues = Map(DesignPath() + "param2" -> IntValue(11))),
      0.5)) // concrete value
    generator.addEvaluatedPoint(rootCompiler) // dummy - ignore

    generator.nextPoint() should equal(None)
  }
}