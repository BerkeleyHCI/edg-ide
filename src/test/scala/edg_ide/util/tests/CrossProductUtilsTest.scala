package edg_ide.util.tests

import edg_ide.util.CrossProductUtils.crossProduct
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CrossProductUtilsTest extends AnyFlatSpec with Matchers {
  behavior of "CrossProductUtils"

  it should "work on empty input" in {
    crossProduct(Seq(Seq())) should equal(Seq())
  }

  it should "work on single input" in {
    crossProduct(Seq(Seq(1))) should equal(Seq(Seq(1)))
  }

  it should "work on single input with multiple elements" in {
    crossProduct(Seq(Seq(1, 2))) should equal(Seq(Seq(1), Seq(2)))
  }

  it should "work on 2x2 inputs including producing ordered outputs" in {
    crossProduct(Seq(Seq(0, 1), Seq(10, 11))) should equal(Seq(
      Seq(0, 10), Seq(0, 11),
      Seq(1, 10), Seq(1, 11)))
  }
}
