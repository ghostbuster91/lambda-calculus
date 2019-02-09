package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Naturals._
import io.ghostbsuter91.lambda.calculus.Booleans._
import org.scalatest.{FlatSpec, Matchers}

class NaturalsTest extends FlatSpec with Matchers {

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

  "plus" should "work" in {
    isEq(plus(One)(One))(Two) shouldBe True
    isEq(plus(One)(Zero))(One) shouldBe True
    isEq(plus(Zero)(Zero))(Zero) shouldBe True
    isEq(plus(Zero)(One))(One) shouldBe True
    isEq(plus(Two)(One))(Three) shouldBe True
  }

  "minus" should "work" in {
    isEq(minus(One)(One))(Zero) shouldBe True
    isEq(minus(One)(Zero))(One) shouldBe True
    isEq(minus(Two)(One))(One) shouldBe True
    isEq(minus(Three)(One))(Two) shouldBe True
    isEq(minus(Two)(Two))(Zero) shouldBe True
  }

  "mult" should "work" in {
    isEq(mult(One)(One))(One) shouldBe True
    isEq(mult(One)(Two))(Two) shouldBe True
    isEq(mult(Two)(One))(Two) shouldBe True
    isEq(mult(Zero)(Two))(Zero) shouldBe True
    isEq(mult(Two)(Zero))(Zero) shouldBe True
  }
}
