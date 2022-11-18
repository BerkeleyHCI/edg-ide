package edg_ide.util.tests

import edg_ide.util.IterableExtensions.IterableExtension
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class IterableExtensionsTest extends AnyFlatSpec with Matchers {
  behavior of "IterableExtensions"

  it should "get allSameValue of list with same value" in {
    Seq(1).allSameValue should equal(Some(1))
    Seq(2, 2).allSameValue should equal(Some(2))
    Seq(4, 4, 4, 4).allSameValue should equal(Some(4))
  }

  it should "get allSameValue of empty list or list with different values" in {
    Seq(1, 2).allSameValue should equal(None)
    Seq(1, 2, 3).allSameValue should equal(None)
    Seq().allSameValue should equal(None)
  }
}
