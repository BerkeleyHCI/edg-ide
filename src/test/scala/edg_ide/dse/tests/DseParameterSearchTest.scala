package edg_ide.dse.tests

import edg.compiler.FloatValue
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.dse.DseParameterSearch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DseParameterSearchTest extends AnyFlatSpec with Matchers {
  behavior of "DseParameterSearch"

  it should "parse String to FloatValue" in {
    val reference = DseParameterSearch(DesignPath(), Seq(FloatValue(1.0)))
    reference.valuesStringToConfig("1.0").get.getValues.map(_._1) should equal(
      Seq(FloatValue(1.0)))
    reference.valuesStringToConfig("1.0, 2.0").get.getValues.map(_._1) should equal(
      Seq(FloatValue(1.0), FloatValue(2.0)))
    reference.valuesStringToConfig("1.0, 2.0,3.0").get.getValues.map(_._1) should equal(
      Seq(FloatValue(1.0), FloatValue(2.0), FloatValue(3.0)))

    reference.valuesStringToConfig("") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig(",") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("ducks") shouldBe a[Errorable.Error]
  }
}
