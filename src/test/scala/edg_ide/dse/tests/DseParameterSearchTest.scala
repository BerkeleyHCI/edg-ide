package edg_ide.dse.tests

import edg.compiler.{BooleanValue, FloatValue, IntValue, RangeValue, TextValue}
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.dse.DseParameterSearch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DseParameterSearchTest extends AnyFlatSpec with Matchers {
  behavior of "DseParameterSearch"

  it should "parse String to BooleanValue" in {
    val reference = DseParameterSearch(DesignPath(), Seq(BooleanValue(true)))
    reference.valuesStringToConfig("true, false").get.getValues.map(_._1) should equal(
      Seq(BooleanValue(true), BooleanValue(false)))
    reference.valuesStringToConfig("false, true").get.getValues.map(_._1) should equal(
      Seq(BooleanValue(false), BooleanValue(true)))
    reference.valuesStringToConfig("true").get.getValues.map(_._1) should equal(
      Seq(BooleanValue(true)))

    reference.valuesStringToConfig("") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig(",") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("ducks") shouldBe a[Errorable.Error]
  }

  it should "parse String to IntValue" in {
    val reference = DseParameterSearch(DesignPath(), Seq(IntValue(1)))
    reference.valuesStringToConfig("1").get.getValues.map(_._1) should equal(
      Seq(IntValue(1)))
    reference.valuesStringToConfig("1, 2").get.getValues.map(_._1) should equal(
      Seq(IntValue(1), IntValue(2)))
    reference.valuesStringToConfig("1, 2,3").get.getValues.map(_._1) should equal(
      Seq(IntValue(1), IntValue(2), IntValue(3)))

    reference.valuesStringToConfig("") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig(",") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("1.0") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("ducks") shouldBe a[Errorable.Error]
  }

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

  it should "parse String to TextValue" in {
    val reference = DseParameterSearch(DesignPath(), Seq(TextValue("")))
    reference.valuesStringToConfig("").get.getValues.map(_._1) should equal(
      Seq(TextValue("")))
    reference.valuesStringToConfig("abc").get.getValues.map(_._1) should equal(
      Seq(TextValue("abc")))
    reference.valuesStringToConfig(",def").get.getValues.map(_._1) should equal(
      Seq(TextValue(""), TextValue("def")))
    reference.valuesStringToConfig("abc,def").get.getValues.map(_._1) should equal(
      Seq(TextValue("abc"), TextValue("def")))
    reference.valuesStringToConfig("abc, def").get.getValues.map(_._1) should equal(
      Seq(TextValue("abc"), TextValue(" def")))
    reference.valuesStringToConfig("abc,\" def\"").get.getValues.map(_._1) should equal(
      Seq(TextValue("abc"), TextValue(" def")))
    reference.valuesStringToConfig("abc,\" def, ghi\"").get.getValues.map(_._1) should equal(
      Seq(TextValue("abc"), TextValue(" def, ghi")))
    reference.valuesStringToConfig("abc,\\\" def, ghi\\\"").get.getValues.map(_._1) should equal(
      Seq(TextValue("abc"), TextValue("\" def"), TextValue(" ghi\"")))
    reference.valuesStringToConfig("a\\\\bc").get.getValues.map(_._1) should equal(
      Seq(TextValue("a\\bc")))

    reference.valuesStringToConfig("abc\"") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("abc,\"") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("abc,\"de") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("abc,de\"de\"") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("abc,de\\") shouldBe a[Errorable.Error]
  }

  it should "parse String to RangeValue" in {
    val reference = DseParameterSearch(DesignPath(), Seq(RangeValue(-1, 1)))
    reference.valuesStringToConfig("(1, 2)").get.getValues.map(_._1) should equal(
      Seq(RangeValue(1, 2)))
    reference.valuesStringToConfig("(-1, 1)").get.getValues.map(_._1) should equal(
      Seq(RangeValue(-1, 1)))
    reference.valuesStringToConfig("(-2.5, 4.2)").get.getValues.map(_._1) should equal(
      Seq(RangeValue(-2.5, 4.2)))
    reference.valuesStringToConfig("(-1, 1), (-2.5, 4.2)").get.getValues.map(_._1) should equal(
      Seq(RangeValue(-1, 1), RangeValue(-2.5, 4.2)))
    reference.valuesStringToConfig("(-1, 1), (-2.5, 4.2),(2,3)").get.getValues.map(_._1) should equal(
      Seq(RangeValue(-1, 1), RangeValue(-2.5, 4.2), RangeValue(2, 3)))

    reference.valuesStringToConfig("") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("(-1, 1, 1)") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("(-1)") shouldBe a[Errorable.Error]
    reference.valuesStringToConfig("(-1), (-1, 2)") shouldBe a[Errorable.Error]
  }
}
