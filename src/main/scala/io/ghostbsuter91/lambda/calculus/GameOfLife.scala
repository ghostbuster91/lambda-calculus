package io.ghostbsuter91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans.{and, or}
import io.ghostbsuter91.lambda.calculus.Numbers.{isEq, next, prev}
import io.ghostbsuter91.lambda.calculus.Sets._

object GameOfLife {
  val _x: F = x => _ => x
  val _y: F = _ => y => y

  def createLife: F = x => y => c => c(x)(y)

  def addLife: F = list => life => add(list)(life)(isSame)

  private def addCopy: F = list => life => xf => yf => addLife(list)(copyLife(life)(xf)(yf))

  def getPossibleNeighbours: F = life => addCopy(addCopy(addCopy(addCopy(addCopy(addCopy(addCopy(addCopy(emptySet)(life)(prev)(prev))(life)(prev)(F.Identity))(life)(prev)(next))(life)(F.Identity)(next))(life)(next)(next))(life)(next)(F.Identity))(life)(next)(prev))(life)(F.Identity)(prev)

  def copyLife: F = life => xf => yf => createLife(xf(life(_x)))(yf(life(_y)))

  def isSame: F = l1 => l2 => and(isEq(l1(_x))(l2(_x)))(isEq(l1(_y))(l2(_y)))

  def countNeighbours: F = lives => life => sSize(sIntersect(getPossibleNeighbours(life))(lives)(isSame))

  def shouldSurvive: F = neighboursCount => or(Naturals.isEq(neighboursCount)(Naturals.Two))(Naturals.isEq(neighboursCount)(Naturals.Three))

  def shouldEmerge: F = neighboursCount => Naturals.isEq(neighboursCount)(Naturals.Three)

  def survivors: F = lives => sFilter(lives)(life => shouldSurvive(countNeighbours(lives)(life)))

  def newLives: F = lives => sFlatMap(lives)(life => sFilter(getPossibleNeighbours(life))(life => shouldEmerge(countNeighbours(lives)(life))))(isSame)

  def tick: F = lives => sAdd(survivors(lives))(newLives(lives))(isSame)
}
