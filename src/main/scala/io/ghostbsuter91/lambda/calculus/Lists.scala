package io.ghostbsuter91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans.{False, True}
import io.ghostbsuter91.lambda.calculus.Naturals.{One, Zero, plus}

object Lists {

  val getValue: F = x => _ => _ => x
  val prevElement: F = _ => y => _ => y
  val isEmpty: F = _ => _ => z => z

  val add: F = list => elem => p => p(elem)(list)(False)

  val emptyList: F = p => p(F.Identity)(F.Identity)(True)

  private def foldFactory: F = r => list => f => acc => FlowControl.ifLambda(list(isEmpty))(_ => acc)(_ => r(list(prevElement))(f)(f(acc)(list(getValue))))

  def fold: F = F.Y(foldFactory)

  def lSize: F = list => fold(list)(acc => _ => plus(acc)(One))(Zero)

  def lSum: F = list => fold(list)(acc => item => plus(acc)(item))(Zero)

  def lAdd: F = list1 => list2 => fold(list2)(acc => item => add(acc)(item))(list1)

  def lFilter: F = list => f => fold(list)(acc => item => FlowControl.ifLambda(f(item))(_ => add(acc)(item))(_ => acc))(emptyList)

  def lMap: F = list => f => lReverse(fold(list)(acc => item => add(acc)(f(item)))(emptyList))

  def lReverse: F = list => fold(list)(acc => item => add(acc)(item))(emptyList)

  def contains: F = list => predicate => Naturals.lessOrEq(Naturals.One)(Lists.lSize(Lists.lFilter(list)(predicate)))
}