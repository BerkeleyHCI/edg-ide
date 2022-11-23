package edg_ide.dse.tests

import edg.compiler.{BooleanValue, ExprValue, FloatValue, IntValue, RangeValue, TextValue}
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.dse.DseParameterSearch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DseParameterSearchTest extends AnyFlatSpec with Matchers {
  behavior of "DseParameterSearch"

  protected def shouldRoundtrip(values: Seq[ExprValue]): Unit = {
    val parameter = DseParameterSearch(DesignPath(), values)
    parameter.valuesStringToConfig(parameter.valuesToString()).get.getValues.map(_._1) should equal(values)
  }

  protected def parseString(examples: Seq[ExprValue], str: String): Errorable[Seq[ExprValue]] = {
    val exampleParameter = DseParameterSearch(DesignPath(), examples)
    exampleParameter.valuesStringToConfig(str).map(_.getValues).map(_.map(_._1))
  }

  it should "parse String to BooleanValue" in {
    val reference = Seq(BooleanValue(true))
    parseString(reference, "true, false").get should equal(Seq(BooleanValue(true), BooleanValue(false)))
    parseString(reference, "false, true").get should equal(Seq(BooleanValue(false), BooleanValue(true)))
    parseString(reference, "true").get should equal(Seq(BooleanValue(true)))

    parseString(reference, "") shouldBe a[Errorable.Error]
    parseString(reference, ",") shouldBe a[Errorable.Error]
    parseString(reference, "ducks") shouldBe a[Errorable.Error]
  }

  it should "roundtrip BooleanValue" in {
    shouldRoundtrip(Seq(BooleanValue(true)))
    shouldRoundtrip(Seq(BooleanValue(false)))
    shouldRoundtrip(Seq(BooleanValue(true), BooleanValue(false)))
    shouldRoundtrip(Seq(BooleanValue(false), BooleanValue(true)))
  }

  it should "parse String to IntValue" in {
    val reference = Seq(IntValue(1))
    parseString(reference, "1").get should equal(Seq(IntValue(1)))
    parseString(reference, "1, 2").get should equal(Seq(IntValue(1), IntValue(2)))
    parseString(reference, "1, 2,3").get should equal(Seq(IntValue(1), IntValue(2), IntValue(3)))

    parseString(reference, "") shouldBe a[Errorable.Error]
    parseString(reference, ",") shouldBe a[Errorable.Error]
    parseString(reference, "1.0") shouldBe a[Errorable.Error]
    parseString(reference, "ducks") shouldBe a[Errorable.Error]
  }

  it should "parse String to FloatValue" in {
    val reference = Seq(FloatValue(1.0))
    parseString(reference, "1.0").get should equal(Seq(FloatValue(1.0)))
    parseString(reference, "1.0, 2.0").get should equal(Seq(FloatValue(1.0), FloatValue(2.0)))
    parseString(reference, "1.0, 2.0,3.0").get should equal(Seq(FloatValue(1.0), FloatValue(2.0), FloatValue(3.0)))

    parseString(reference, "") shouldBe a[Errorable.Error]
    parseString(reference, ",") shouldBe a[Errorable.Error]
    parseString(reference, "ducks") shouldBe a[Errorable.Error]
  }

  it should "parse String to TextValue" in {
    val reference = Seq(TextValue(""))
    parseString(reference, "").get should equal(Seq(TextValue("")))
    parseString(reference, "abc").get should equal(Seq(TextValue("abc")))
    parseString(reference, ",def").get should equal(Seq(TextValue(""), TextValue("def")))
    parseString(reference, "abc,def").get should equal(Seq(TextValue("abc"), TextValue("def")))
    parseString(reference, "abc, def").get should equal(Seq(TextValue("abc"), TextValue(" def")))
    parseString(reference, "abc,\" def\"").get should equal(Seq(TextValue("abc"), TextValue(" def")))
    parseString(reference, "abc,\" def, ghi\"").get should equal(Seq(TextValue("abc"), TextValue(" def, ghi")))
    parseString(reference, "abc,\\\" def, ghi\\\"").get should equal(
      Seq(TextValue("abc"), TextValue("\" def"), TextValue(" ghi\"")))
    parseString(reference, "a\\\\bc").get should equal(Seq(TextValue("a\\bc")))

    parseString(reference, "abc\"") shouldBe a[Errorable.Error]
    parseString(reference, "abc,\"") shouldBe a[Errorable.Error]
    parseString(reference, "abc,\"de") shouldBe a[Errorable.Error]
    parseString(reference, "abc,de\"de\"") shouldBe a[Errorable.Error]
    parseString(reference, "abc,de\\") shouldBe a[Errorable.Error]
  }

  it should "parse String to RangeValue" in {
    val reference = Seq(RangeValue(-1, 1))
    parseString(reference, "(1, 2)").get should equal(Seq(RangeValue(1, 2)))
    parseString(reference, "(-1, 1)").get should equal(Seq(RangeValue(-1, 1)))
    parseString(reference, "(-2.5, 4.2)").get should equal(Seq(RangeValue(-2.5, 4.2)))
    parseString(reference, "(-1, 1), (-2.5, 4.2)").get should equal(
      Seq(RangeValue(-1, 1), RangeValue(-2.5, 4.2)))
    parseString(reference, "(-1, 1), (-2.5, 4.2),(2,3)").get should equal(
      Seq(RangeValue(-1, 1), RangeValue(-2.5, 4.2), RangeValue(2, 3)))

    parseString(reference, "") shouldBe a[Errorable.Error]
    parseString(reference, "(-1, 1, 1)") shouldBe a[Errorable.Error]
    parseString(reference, "(-1)") shouldBe a[Errorable.Error]
    parseString(reference, "(-1), (-1, 2)") shouldBe a[Errorable.Error]
  }
}
