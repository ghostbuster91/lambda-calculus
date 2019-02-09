package io.ghostbuster91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.GameOfLife._
import io.ghostbsuter91.lambda.calculus.Numbers._
import io.ghostbsuter91.lambda.calculus.Sets._
import io.ghostbsuter91.lambda.calculus.{F, Naturals}
import org.scalatest.{FlatSpec, Matchers}

class GameOfLifeTest extends FlatSpec with Matchers {
  val nEight: F = Naturals.pow(Naturals.Two)(Naturals.Three)

  "life" should "have properties" in {
    val life = createLife(pTwo)(pThree)
    isEq(life(_x))(pTwo) shouldBe True
    isEq(life(_y))(pThree) shouldBe True
  }

  "life" should "be comparable" in {
    val l1 = createLife(pTwo)(pThree)
    val l2 = createLife(pOne)(pThree)
    val l3 = createLife(pTwo)(pThree)

    isSame(l1)(l2) shouldBe False
    isSame(l1)(l3) shouldBe True
    isSame(l3)(l1) shouldBe True
    isSame(l3)(l2) shouldBe False
  }

  it should "have possible neighbours" in {
    val life = createLife(pZero)(pZero)
    val neighbours = getPossibleNeighbours(life)
    sContains(neighbours)(isSame(createLife(nOne)(nOne))) shouldBe True
    sContains(neighbours)(isSame(createLife(nOne)(pZero))) shouldBe True
    sContains(neighbours)(isSame(createLife(nOne)(pOne))) shouldBe True
    sContains(neighbours)(isSame(createLife(pZero)(pOne))) shouldBe True
    sContains(neighbours)(isSame(createLife(pOne)(pOne))) shouldBe True
    sContains(neighbours)(isSame(createLife(pOne)(pZero))) shouldBe True
    sContains(neighbours)(isSame(createLife(pOne)(nOne))) shouldBe True
    sContains(neighbours)(isSame(createLife(pZero)(nOne))) shouldBe True
    Naturals.isEq(sSize(neighbours))(nEight) shouldBe True
  }

  it should "count neighbours" in {
    val life = createLife(pZero)(pZero)
    val lives = getPossibleNeighbours(life)
    Naturals.isEq(countNeighbours(lives)(life))(nEight) shouldBe True

    val life2 = createLife(pOne)(pOne)
    val lives2 = getPossibleNeighbours(life2)
    Naturals.isEq(countNeighbours(lives2)(life))(Naturals.Two) shouldBe True
  }

  "life" should "die if alone" in {
    val life = createLife(pZero)(pZero)
    val lives = addLife(emptySet)(life)

    tick(lives)(_isEmpty) shouldBe True
  }

  "life" should "remain alive when it has two neighbours" in {
    val life = createLife(pZero)(pZero)
    val lives = addLife(addLife(addLife(emptySet)(life))(createLife(pOne)(pZero)))(createLife(nOne)(pZero))

    val results = tick(lives)
    results(_isEmpty) shouldBe False

    sContains(results)(isSame(life)) shouldBe True
  }

  "life" should "remain alive when it has three neighbours" in {
    val life = createLife(pZero)(pZero)
    val lives = addLife(addLife(addLife(addLife(emptySet)(life))(createLife(pOne)(pZero)))(createLife(nOne)(pZero)))(createLife(pZero)(pOne))

    val results = tick(lives)
    results(_isEmpty) shouldBe False

    sContains(results)(isSame(life)) shouldBe True
  }

  "life" should "die if it has more than three neighbours" in {
    val life = createLife(pZero)(pZero)
    val lives = addLife(addLife(addLife(addLife(addLife(emptySet)(life))(createLife(pOne)(pZero)))(createLife(nOne)(pZero)))(createLife(pZero)(pOne)))(createLife(pZero)(nOne))

    val results = tick(lives)
    results(_isEmpty) shouldBe False

    sContains(results)(isSame(life)) shouldBe False
  }

  "life" should "emerge when it has exactly three neighbours" in {
    val lives = addLife(addLife(addLife(emptySet)(createLife(nOne)(nOne)))(createLife(nOne)(pOne)))(createLife(pOne)(pZero))
    val result = tick(lives)

    sContains(result)(isSame(createLife(pZero)(pZero))) shouldBe True
    Naturals.isEq(sSize(result))(Naturals.One) shouldBe True
  }

  "square" should "be immortal" in {
    val lives = addLife(addLife(addLife(addLife(emptySet)(createLife(pZero)(pZero)))(createLife(pOne)(pOne)))(createLife(pOne)(pZero)))(createLife(pZero)(pOne))
    val result = tick(tick(tick(tick(lives))))

    val Four = Naturals.mult(Naturals.Two)(Naturals.Two)
    Naturals.isEq(sSize(result))(Four) shouldBe True
  }

  "three in a row" should "flip for ever" in {
    val horizontalRow = addLife(addLife(addLife(emptySet)(createLife(pZero)(pZero)))(createLife(nOne)(pZero)))(createLife(pOne)(pZero))
    val verticalRow = addLife(addLife(addLife(emptySet)(createLife(pZero)(pZero)))(createLife(pZero)(nOne)))(createLife(pZero)(pOne))
    sEq(verticalRow)(horizontalRow)(isSame) shouldBe False

    val firstTick = tick(horizontalRow)
    sEq(firstTick)(verticalRow)(isSame) shouldBe True

    val secondTick = tick(firstTick)
    sEq(secondTick)(horizontalRow)(isSame) shouldBe True
  }
}


