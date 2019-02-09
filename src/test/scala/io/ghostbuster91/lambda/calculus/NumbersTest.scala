package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.Numbers._
import org.scalatest.{FlatSpec, Matchers}

class NumbersTest extends FlatSpec with Matchers {

  "isZero" should "work" in {
    isZero(Zero) shouldBe True
    isZero(One) shouldBe False
    isZero(Two) shouldBe False
    isZero(Three) shouldBe False
  }

  "positive numbers" should "be positive" in {
    One(_isPositive) shouldBe True
    Two(_isPositive) shouldBe True
    Three(_isPositive) shouldBe True
  }

  "negative numbers" should "be negative" in {
    negative(One)(_isPositive) shouldBe False
    negative(Two)(_isPositive) shouldBe False
    negative(Three)(_isPositive) shouldBe False
  }

  "negation" should "be reversible" in {
    negative(negative(One))(_isPositive) shouldBe True
  }

  "zero" should "be always positive" in {
    negative(Zero)(_isPositive) shouldBe True
    negative(negative(Zero))(_isPositive) shouldBe True
    isZero(negative(Zero)) shouldBe True
  }

  "isEqual" should "work" in {
    isEq(One)(One) shouldBe True
    isEq(Two)(One) shouldBe False
    isEq(Two)(Two) shouldBe True
    isEq(nOne)(nOne) shouldBe True
    isEq(One)(nOne) shouldBe False
    isEq(negative(Two))(negative(Two)) shouldBe True
  }

  "next" should "work for positive numbers" in {
    isEq(next(One))(Two) shouldBe True
    isEq(next(Zero))(One) shouldBe True
    isEq(next(Two))(Three) shouldBe True
  }

  "next" should "work for negative numbers" in {
    isEq(next(nTwo))(nOne) shouldBe True
    isEq(next(nOne))(Zero) shouldBe True
  }

  "plus" should "work for both positive numbers" in {
    isEq(plus(One)(One))(Two) shouldBe True
    isEq(plus(Zero)(One))(One) shouldBe True
    isEq(plus(Two)(Zero))(Two) shouldBe True
  }

  "plus" should "work for both negative numbers" in {
    isEq(plus(nOne)(nOne))(nTwo) shouldBe True
    isEq(plus(nTwo)(nOne))(nThree) shouldBe True
  }

  "plus" should "work with mixed numbers" in {
    isEq(plus(nTwo)(One))(nOne) shouldBe True
    isEq(plus(One)(nTwo))(nOne) shouldBe True
    isEq(plus(nTwo)(Zero))(nTwo) shouldBe True
    isEq(plus(nTwo)(Two))(Zero) shouldBe True
    isEq(plus(Two)(nTwo))(Zero) shouldBe True
  }

  "minus" should "work" in {
    isEq(minus(One)(One))(Zero) shouldBe True
    isEq(minus(One)(Two))(nOne) shouldBe True
    isEq(minus(nOne)(One))(nTwo) shouldBe True
    isEq(minus(nTwo)(nOne))(nOne) shouldBe True
  }

  "prev" should "work" in {
    isEq(prev(Two))(One) shouldBe True
    isEq(prev(One))(Zero) shouldBe True
    isEq(prev(Zero))(nOne) shouldBe True
    isEq(prev(nOne))(nTwo) shouldBe True
  }

  "mult" should "work for positive numbers" in {
    isEq(mult(One)(One))(One) shouldBe True
    isEq(mult(One)(Two))(Two) shouldBe True
    isEq(mult(Two)(One))(Two) shouldBe True
    isEq(mult(Zero)(Two))(Zero) shouldBe True
    isEq(mult(Two)(Zero))(Zero) shouldBe True
  }

  "mult" should "work for negative numbers" in {
    isEq(mult(nOne)(nOne))(One) shouldBe True
    isEq(mult(nOne)(nTwo))(Two) shouldBe True
    isEq(mult(nTwo)(nOne))(Two) shouldBe True
  }

  "mult" should "work for mixed numbers" in {
    isEq(mult(Zero)(nTwo))(Zero) shouldBe True
    isEq(mult(Two)(Zero))(Zero) shouldBe True
    isEq(mult(nTwo)(One))(nTwo) shouldBe True
    isEq(mult(nOne)(One))(nOne) shouldBe True
  }
}
