package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans
import io.ghostbsuter91.lambda.calculus.Numbers._
import io.ghostbsuter91.lambda.calculus.Recursive._
import org.scalatest.{FlatSpec, Matchers}

class Factorial extends FlatSpec with Matchers {

  "factorial" should "work" in {
    isEq(factorial(Three))(next(next(next(Three)))) shouldBe Booleans.True
    isEq(factorial(Two))(Two) shouldBe Booleans.True
  }

  "fibonacci" should "work" in {
    val Five = next(next(Three))
    isEq(fibonacci(Zero))(Zero) shouldBe Booleans.True
    isEq(fibonacci(One))(One) shouldBe Booleans.True
    isEq(fibonacci(Two))(One) shouldBe Booleans.True
    isEq(fibonacci(Three))(Two) shouldBe Booleans.True
    isEq(fibonacci(next(Three)))(Three) shouldBe Booleans.True
    isEq(fibonacci(Five))(Five) shouldBe Booleans.True
  }
}
