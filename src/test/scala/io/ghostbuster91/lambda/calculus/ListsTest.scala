package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.F
import io.ghostbsuter91.lambda.calculus.Lists._
import io.ghostbsuter91.lambda.calculus.Numbers._
import org.scalatest.{FlatSpec, Matchers}

class ListsTest extends FlatSpec with Matchers {

  "fold" should "perform map with identity" in {
    val list = add(emptyList)(One)
    val list2 = add(list)(Two)

    emptyList(isEmpty) shouldBe True
    isEq(list2(getValue))(Two) shouldBe True
    isEq(list2(prevElement)(getValue))(One) shouldBe True
    list2(isEmpty) shouldBe False

    val list3 = lMap(list2)(F.Identity)
    isEq(list3(getValue))(Two) shouldBe True
    isEq(list3(prevElement)(getValue))(One) shouldBe True
    list3(prevElement)(prevElement)(isEmpty) shouldBe True
  }

  "fold" should "map with complex function" in {
    val list = add(emptyList)(One)
    val list2 = add(list)(Two)

    emptyList(isEmpty) shouldBe True
    isEq(list2(getValue))(Two) shouldBe True
    isEq(list2(prevElement)(getValue))(One) shouldBe True
    list2(isEmpty) shouldBe False

    val plusOne: F = n => plus(n)(One)
    val list3 = lMap(list2)(plusOne)

    isEq(list3(getValue))(Three) shouldBe True
    list3(isEmpty) shouldBe False
    isEq(list3(prevElement)(getValue))(Two) shouldBe True
    list3(prevElement)(prevElement)(isEmpty) shouldBe True
  }

  "fold" should "filter" in {
    val list = add(emptyList)(One)
    val list2 = add(list)(Two)

    emptyList(isEmpty) shouldBe True
    isEq(list2(getValue))(Two) shouldBe True
    isEq(list2(prevElement)(getValue))(One) shouldBe True
    list2(isEmpty) shouldBe False

    val isOne: F = n => isEq(n)(One)
    val list3 = lFilter(list2)(isOne)

    isEq(list3(getValue))(One) shouldBe True
    list3(isEmpty) shouldBe False
    list3(prevElement)(isEmpty) shouldBe True
  }

  "fold" should "sum elements" in {
    val list: F = add(emptyList)(One)
    val list2 = add(list)(Two)

    emptyList(isEmpty) shouldBe True
    isEq(list2(getValue))(Two) shouldBe True
    isEq(list2(prevElement)(getValue))(One) shouldBe True
    list2(isEmpty) shouldBe False

    val result = lSum(list2)

    isEq(result)(Three) shouldBe True
  }

  "fold" should "count number of elements" in {
    val list = add(emptyList)(One)
    val list2 = add(list)(Two)
    val list3 = add(add(add(add(list2)(Two))(Two))(Two))(Three)

    emptyList(isEmpty) shouldBe True
    isEq(list2(getValue))(Two) shouldBe True
    isEq(list2(prevElement)(getValue))(One) shouldBe True
    list2(isEmpty) shouldBe False

    val result = lSize(list3)

    isEq(result)(plus(Three)(Three)) shouldBe True
  }

  "fold" should "add two lists" in {
    val four = plus(Three)(One)
    val list1 = add(add(add(add(emptyList)(Two))(Two))(Two))(Three)
    val list2 = add(add(add(add(emptyList)(Two))(Two))(Two))(Three)

    val size1 = lSize(list1)
    val size2 = lSize(list2)
    isEq(size1)(four) shouldBe True
    isEq(size2)(four) shouldBe True

    val result = lSize(lAdd(list1)(list2))
    isEq(result)(plus(four)(four)) shouldBe True
  }
}

