package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.Numbers._
import org.scalatest.{FlatSpec, Matchers}

class NumbersTest extends FlatSpec with Matchers {

  "isZero" should "work" in {
    isZero(pZero) shouldBe True
    isZero(pOne) shouldBe False
    isZero(pTwo) shouldBe False
    isZero(pThree) shouldBe False
  }

  "positive numbers" should "be positive" in {
    pOne(_isPositive) shouldBe True
    pTwo(_isPositive) shouldBe True
    pThree(_isPositive) shouldBe True
  }

  "negative numbers" should "be negative" in {
    negative(pOne)(_isPositive) shouldBe False
    negative(pTwo)(_isPositive) shouldBe False
    negative(pThree)(_isPositive) shouldBe False
  }

  "negation" should "be reversible" in {
    negative(negative(pOne))(_isPositive) shouldBe True
  }

  "zero" should "be always positive" in {
    negative(pZero)(_isPositive) shouldBe True
    negative(negative(pZero))(_isPositive) shouldBe True
    isZero(negative(pZero)) shouldBe True
  }

  "isEqual" should "work" in {
    isEq(pOne)(pOne) shouldBe True
    isEq(pTwo)(pOne) shouldBe False
    isEq(pTwo)(pTwo) shouldBe True
    isEq(nOne)(nOne) shouldBe True
    isEq(pOne)(nOne) shouldBe False
    isEq(negative(pTwo))(negative(pTwo)) shouldBe True
  }

  "next" should "work for positive numbers" in {
    isEq(next(pOne))(pTwo) shouldBe True
    isEq(next(pZero))(pOne) shouldBe True
    isEq(next(pTwo))(pThree) shouldBe True
  }

  "next" should "work for negative numbers" in {
    isEq(next(nTwo))(nOne) shouldBe True
    isEq(next(nOne))(pZero) shouldBe True
  }

  "plus" should "work for both positive numbers" in {
    isEq(plus(pOne)(pOne))(pTwo) shouldBe True
    isEq(plus(pZero)(pOne))(pOne) shouldBe True
    isEq(plus(pTwo)(pZero))(pTwo) shouldBe True
  }

  "plus" should "work for both negative numbers" in {
    isEq(plus(nOne)(nOne))(nTwo) shouldBe True
    isEq(plus(nTwo)(nOne))(nThree) shouldBe True
  }

  "plus" should "work with mixed numbers" in {
    isEq(plus(nTwo)(pOne))(nOne) shouldBe True
    isEq(plus(pOne)(nTwo))(nOne) shouldBe True
    isEq(plus(nTwo)(pZero))(nTwo) shouldBe True
    isEq(plus(nTwo)(pTwo))(pZero) shouldBe True
    isEq(plus(pTwo)(nTwo))(pZero) shouldBe True
  }

  "minus" should "work" in {
    isEq(minus(pOne)(pOne))(pZero) shouldBe True
    isEq(minus(pOne)(pTwo))(nOne) shouldBe True
    isEq(minus(nOne)(pOne))(nTwo) shouldBe True
    isEq(minus(nTwo)(nOne))(nOne) shouldBe True
  }

  "prev" should "work" in {
    isEq(prev(pTwo))(pOne) shouldBe True
    isEq(prev(pOne))(pZero) shouldBe True
    isEq(prev(pZero))(nOne) shouldBe True
    isEq(prev(nOne))(nTwo) shouldBe True
  }

  "mult" should "work for positive numbers" in {
    isEq(mult(pOne)(pOne))(pOne) shouldBe True
    isEq(mult(pOne)(pTwo))(pTwo) shouldBe True
    isEq(mult(pTwo)(pOne))(pTwo) shouldBe True
    isEq(mult(pZero)(pTwo))(pZero) shouldBe True
    isEq(mult(pTwo)(pZero))(pZero) shouldBe True
  }

  "mult" should "work for negative numbers" in {
    isEq(mult(nOne)(nOne))(pOne) shouldBe True
    isEq(mult(nOne)(nTwo))(pTwo) shouldBe True
    isEq(mult(nTwo)(nOne))(pTwo) shouldBe True
  }

  "mult" should "work for mixed numbers" in {
    isEq(mult(pZero)(nTwo))(pZero) shouldBe True
    isEq(mult(pTwo)(pZero))(pZero) shouldBe True
    isEq(mult(nTwo)(pOne))(nTwo) shouldBe True
    isEq(mult(nOne)(pOne))(nOne) shouldBe True
  }
}
