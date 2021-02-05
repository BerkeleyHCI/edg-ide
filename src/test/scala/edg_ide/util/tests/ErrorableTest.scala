package edg_ide.util.tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import edg_ide.util.Errorable


class ErrorableTest extends AnyFlatSpec with Matchers {
  behavior of "Errorable"

  it should "chain successes" in {
    val original = Errorable(BigInt(1), "failure1")
    val next2 = original.mapWithErr("failure2") {
      _ + 2
    }
    val next3 = next2.mapWithErr("failure3") {
      _ + 3
    }
    next3 should equal(Errorable.Success(BigInt(6)))
  }

  it should "report null as failure by default" in {
    val err = Errorable(null, "failure1")
    err should equal(Errorable.Error("failure1"))
  }

  it should "preserve first failure" in {
    val original = Errorable(BigInt(1), "failure1")
    val next2 = original.mapWithErr("failure2") {
      prev => null.asInstanceOf[BigInt]
    }
    val next3 = next2.mapWithErr("failure3") {
      _ + 3
    }
    next3 should equal(Errorable.Error("failure2"))
  }
}
