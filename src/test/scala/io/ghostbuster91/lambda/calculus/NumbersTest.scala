package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Numbers._
import io.ghostbsuter91.lambda.calculus.Booleans._
import org.scalatest.{FlatSpec, Matchers}

class NumbersTest extends FlatSpec with Matchers {

  "isZero" should "work" in {
    isZero(Zero) shouldBe True
    isZero(One) shouldBe False
    isZero(Two) shouldBe False
    isZero(Three) shouldBe False
  }

  "lessOrEq" should "work" in {
    lessOrEq(Zero)(One) shouldBe True
    lessOrEq(One)(One) shouldBe True
    lessOrEq(Two)(Two) shouldBe True
    lessOrEq(One)(Two) shouldBe True
    lessOrEq(Two)(One) shouldBe False
  }

  "next" should "work" in {
    lessOrEq(Zero)(next(Zero)) shouldBe True
    lessOrEq(next(Zero))(Zero) shouldBe False
  }

  "prev" should "work" in {
    lessOrEq(prev(One))(Zero) shouldBe True
    lessOrEq(prev(Two))(Zero) shouldBe False
    lessOrEq(Zero)(prev(One)) shouldBe True
  }

  "eq" should "work" in {
    isEq(One)(One) shouldBe True
    isEq(Two)(One) shouldBe False
    isEq(One)(Two) shouldBe False
    isEq(next(One))(prev(Three)) shouldBe True
  }
}
