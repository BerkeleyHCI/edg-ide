package edg_ide.util.tests

import edg.ElemBuilder
import edg.compiler.RangeValue
import edg.wir.DesignPath
import edg_ide.dse.{DseParameterSearch, DseSubclassSearch}
import edg_ide.util.ObjectSerializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ObjectSerializerTest extends AnyFlatSpec with Matchers {
  "ObjectSerializer" should "roundtrip an example config object" in {
    val exampleConfig = Seq(
      DseSubclassSearch(DesignPath() + "reg_5v",
        Seq(
          "electronics_lib.BuckConverter_TexasInstruments.Tps561201",
          "electronics_lib.BuckConverter_TexasInstruments.Tps54202h",
        ).map(value => ElemBuilder.LibraryPath(value))
      ),
      DseParameterSearch(DesignPath() + "reg_5v" + "ripple_current_factor",
        Seq(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5).map(value => RangeValue(value - 0.05, value + 0.05))
      ),
    )
    ObjectSerializer.deserialize(ObjectSerializer.serialize(exampleConfig)) should equal(Some(exampleConfig))
  }

  "ObjectSerializer.optionInstanceOfSeq" should "properly test types" in {
    ObjectSerializer.optionInstanceOfSeq("ducks").isDefined shouldBe false
    ObjectSerializer.optionInstanceOfSeq[String](Seq("ducks")).isDefined shouldBe true
    ObjectSerializer.optionInstanceOfSeq[Integer](Seq("ducks")).isDefined shouldBe false
    ObjectSerializer.optionInstanceOfSeq[Integer](Seq(2)).isDefined shouldBe true
    ObjectSerializer.optionInstanceOfSeq[(Integer, String)](Seq((2, "ducks"))).isDefined shouldBe true

    // this is a false negative because of type erasure, where type parameters are not checked
    ObjectSerializer.optionInstanceOfSeq[(Integer, String)](Seq(("ducks", 2))).isDefined shouldBe true
    // so we need to give it logic for an inner check
    ObjectSerializer.optionInstanceOfSeq[(Integer, String)](
      Seq(("ducks", 2)),
      { elt: (Integer, String) => elt._1.isInstanceOf[Integer] && elt._2.isInstanceOf[String] }
    ).isDefined shouldBe false
    ObjectSerializer.optionInstanceOfSeq[(Integer, String)](
      Seq((2, "ducks")),
      { elt: (Integer, String) => elt._1.isInstanceOf[Integer] && elt._2.isInstanceOf[String] }
    ).isDefined shouldBe true
  }
}
