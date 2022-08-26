package edg_ide.util.tests

import edg_ide.util.AreaUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class AreaUtilsTest extends AnyFlatSpec with Matchers {
  behavior of "AreaUtil"

  it should "find closed path of square" in {
    AreaUtils.closedPathOf(Seq(
      ((0.0, 0.0), (1.0, 0.0)),
      ((1.0, 0.0), (1.0, 1.0)),
      ((1.0, 1.0), (0.0, 1.0)),
      ((0.0, 1.0), (0.0, 0.0)),
    )) should equal(Some(Seq(
      (0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0),
    )))
  }

  it should "find closed path of square from unordered lines" in {
    AreaUtils.closedPathOf(Seq(
      ((0.0, 0.0), (1.0, 0.0)),
      ((1.0, 0.0), (1.0, 1.0)),
      ((0.0, 1.0), (1.0, 1.0)),  // edge reversed
      ((0.0, 1.0), (0.0, 0.0)),
    )) should equal(Some(Seq(
      (0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0),
    )))
    AreaUtils.closedPathOf(Seq(
      ((0.0, 0.0), (1.0, 0.0)),
      ((0.0, 1.0), (1.0, 1.0)), // edge reversed and shifted
      ((1.0, 0.0), (1.0, 1.0)),
      ((0.0, 1.0), (0.0, 0.0)),
    )) should equal(Some(Seq(
      (0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0),
    )))
  }

  it should "reject invalid paths" in {
    AreaUtils.closedPathOf(Seq(
      ((0.0, 0.0), (1.0, 0.0)),
      ((1.0, 0.0), (1.0, 1.0)),
      ((0.0, 1.0), (0.0, 0.0)),  // non-closed path
    )) should equal(None)
    AreaUtils.closedPathOf(Seq(
      ((0.0, 0.0), (1.0, 0.0)),
      ((1.0, 0.0), (1.0, 1.0)),
      ((1.0, 1.0), (0.0, 1.0)),
      ((0.0, 1.0), (0.0, 0.0)),
      ((1.0, 1.0), (2.0, 2.0)),  // extraneous edge
    )) should equal(None)
  }
}
