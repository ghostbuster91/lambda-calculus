package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.F
import io.ghostbsuter91.lambda.calculus.Sets._
import io.ghostbsuter91.lambda.calculus.Naturals._
import org.scalatest.{FlatSpec, Matchers}

class SetsTest extends FlatSpec with Matchers {

  "fold" should "perform map with identity" in {
    val set = add(emptySet)(One)(isEq)
    val set2 = add(set)(Two)(isEq)

    emptySet(_isEmpty) shouldBe True
    isEq(set2(_value))(Two) shouldBe True
    isEq(set2(_prev)(_value))(One) shouldBe True
    set2(_isEmpty) shouldBe False

    val set3 = sMap(set2)(F.Identity)(isEq)
    sContains(set3)(n => isEq(n)(Two)) shouldBe True
    sContains(set3)(n => isEq(n)(One)) shouldBe True
    isEq(sSize(set3))(Two) shouldBe True
  }

  "fold" should "map with complex function" in {
    val set = add(emptySet)(One)(isEq)
    val set2 = add(set)(Two)(isEq)

    emptySet(_isEmpty) shouldBe True
    isEq(set2(_value))(Two) shouldBe True
    isEq(set2(_prev)(_value))(One) shouldBe True
    set2(_isEmpty) shouldBe False

    val plusOne: F = n => plus(n)(One)
    val set3 = sMap(set2)(plusOne)(isEq)

    sContains(set3)(n => isEq(n)(Three)) shouldBe True
    sContains(set3)(n => isEq(n)(Two)) shouldBe True
    sContains(set3)(n => isEq(n)(One)) shouldBe False
    isEq(sSize(set3))(Two) shouldBe True
  }

  "fold" should "filter" in {
    val set = add(emptySet)(One)(isEq)
    val set2 = add(set)(Two)(isEq)

    emptySet(_isEmpty) shouldBe True
    isEq(set2(_value))(Two) shouldBe True
    isEq(set2(_prev)(_value))(One) shouldBe True
    set2(_isEmpty) shouldBe False

    val isOne: F = n => isEq(n)(One)
    val set3 = sFilter(set2)(isOne)

    isEq(set3(_value))(One) shouldBe True
    set3(_isEmpty) shouldBe False
    set3(_prev)(_isEmpty) shouldBe True
  }

  "fold" should "sum elements" in {
    val set = add(emptySet)(One)(isEq)
    val set2 = add(set)(Two)(isEq)

    emptySet(_isEmpty) shouldBe True
    isEq(set2(_value))(Two) shouldBe True
    isEq(set2(_prev)(_value))(One) shouldBe True
    set2(_isEmpty) shouldBe False

    val result = sSum(set2)

    isEq(result)(Three) shouldBe True
  }

  "fold" should "count number of elements" in {
    val set = add(emptySet)(One)(isEq)
    val set2 = add(set)(Two)(isEq)
    val set3 = add(add(add(add(set2)(Two)(isEq))(Two)(isEq))(Two)(isEq))(Three)(isEq)

    emptySet(_isEmpty) shouldBe True
    sContains(set2)(n => isEq(n)(Two)) shouldBe True
    sContains(set2)(n => isEq(n)(One)) shouldBe True
    set2(_isEmpty) shouldBe False

    val result = sSize(set3)

    isEq(result)(Three) shouldBe True
  }

  "fold" should "add two sets" in {
    val set1 = add(add(add(add(emptySet)(One)(isEq))(Two)(isEq))(Two)(isEq))(Two)(isEq)
    val set2 = add(add(add(add(emptySet)(Two)(isEq))(Two)(isEq))(Two)(isEq))(Three)(isEq)

    val size1 = sSize(set1)
    val size2 = sSize(set2)
    isEq(size1)(Two) shouldBe True
    isEq(size2)(Two) shouldBe True

    val result = sSize(sAdd(set1)(set2)(isEq))
    isEq(result)(Three) shouldBe True
  }

  "set" should "contain elements" in {
    val set = add(emptySet)(Three)(isEq)
    sContains(set)(item => isEq(item)(Three)) shouldBe True
    sContains(set)(item => isEq(item)(Two)) shouldBe False
  }

  "set equality" should "work" in {
    val set1 = add(add(emptySet)(Three)(isEq))(One)(isEq)
    val set2 = add(add(emptySet)(One)(isEq))(Three)(isEq)
    val set3 = add(set2)(Two)(isEq)
    val set4 = add(add(emptySet)(Two)(isEq))(Three)(isEq)

    sEq(set1)(set2)(isEq) shouldBe True
    sEq(set1)(set3)(isEq) shouldBe False
    sEq(set2)(set1)(isEq) shouldBe True
    sEq(set2)(set4)(isEq) shouldBe False
  }

  "flatten" should "work" in {
    val Four = next(Three)
    val set1 = add(add(emptySet)(One)(isEq))(Two)(isEq)
    val set2 = add(add(emptySet)(Three)(isEq))(Four)(isEq)
    val setOfNumbersEquality:F = s1 => s2 => sEq(s1)(s2)(isEq)

    sEq(set1)(set2)(isEq) shouldBe False

    val superSet = add(add(emptySet)(set1)(setOfNumbersEquality))(set2)(setOfNumbersEquality)

    val expectedResult = add(add(add(add(emptySet)(One)(isEq))(Two)(isEq))(Three)(isEq))(Four)(isEq)
    isEq(sSize(expectedResult))(Four) shouldBe True
    isEq(sSize(superSet))(Two) shouldBe True

    sEq(sAdd(set1)(set2)(isEq))(expectedResult)(isEq) shouldBe True
    sEq(sFlatten(superSet)(isEq))(expectedResult)(isEq) shouldBe True
  }

  "flatten" should "eliminates duplicates" in {
    val set1 = add(add(emptySet)(One)(isEq))(Two)(isEq)
    val set2 = add(add(emptySet)(Two)(isEq))(Three)(isEq)
    val setOfNumbersEquality:F = s1 => s2 => sEq(s1)(s2)(isEq)

    sEq(set1)(set2)(isEq) shouldBe False

    val superSet = add(add(emptySet)(set1)(setOfNumbersEquality))(set2)(setOfNumbersEquality)

    val expectedResult = add(add(add(emptySet)(One)(isEq))(Two)(isEq))(Three)(isEq)
    isEq(sSize(expectedResult))(Three) shouldBe True
    isEq(sSize(superSet))(Two) shouldBe True

    sEq(sAdd(set1)(set2)(isEq))(expectedResult)(isEq) shouldBe True
    sEq(sFlatten(superSet)(isEq))(expectedResult)(isEq) shouldBe True
  }
}

