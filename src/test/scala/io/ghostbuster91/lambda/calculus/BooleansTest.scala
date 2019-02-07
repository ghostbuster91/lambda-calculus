package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class BooleansTest extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  "and" should "work" in {
    val cases = Table(
      ("p", "q", "p and q"),
      (False, False, False),
      (False, True, False),
      (True, False, False),
      (True, True, True)
    )

    forAll(cases) {
      case (p, q, pAndQ) =>
        and(p)(q) shouldBe pAndQ
    }
  }

  "or" should "work" in {
    val cases = Table(
      ("p", "q", "p or q"),
      (False, False, False),
      (False, True, True),
      (True, False, True),
      (True, True, True)
    )

    forAll(cases) {
      case (p, q, pOrQ) =>
        or(p)(q) shouldBe pOrQ
    }
  }

  "not" should "work" in {
    val cases = Table(
      ("p", "~p"),
      (False, True),
      (True, False)
    )

    forAll(cases) {
      case (p, negP) =>
        neg(p) shouldBe negP
    }
  }

  "xor" should "work" in {
    val cases = Table(
      ("p", "q", "p xor q"),
      (False, False, True),
      (False, True, False),
      (True, False, False),
      (True, True, True)
    )

    forAll(cases) {
      case (p, q, pXorQ) =>
        xor(p)(q) shouldBe pXorQ
    }
  }
}
