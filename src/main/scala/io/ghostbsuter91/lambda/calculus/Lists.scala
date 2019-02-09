package io.ghostbsuter91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.Naturals._

object Lists {

  val _value: F = x => _ => _ => x
  val _prev: F = _ => y => _ => y
  val _isEmpty: F = _ => _ => z => z

  val add: F = list => elem => p => p(elem)(list)(False)

  val emptyList: F = p => p(F.Identity)(F.Identity)(True)

  private def foldFactory: F = r => list => f => acc => FlowControl.ifLambda(list(_isEmpty))(_ => acc)(_ => r(list(_prev))(f)(f(acc)(list(_value))))

  def fold: F = F.Y(foldFactory)

  def lSize: F = list => fold(list)(acc => _ => plus(acc)(One))(Zero)

  def lSum: F = list => fold(list)(acc => item => plus(acc)(item))(Zero)

  def lAdd: F = list1 => list2 => fold(list2)(acc => item => add(acc)(item))(list1)

  def lFilter: F = list => f => fold(list)(acc => item => FlowControl.ifLambda(f(item))(_ => add(acc)(item))(_ => acc))(emptyList)

  def lMap: F = list => f => lReverse(fold(list)(acc => item => add(acc)(f(item)))(emptyList))

  def lReverse: F = list => fold(list)(acc => item => add(acc)(item))(emptyList)

  def lContains: F = list => predicate => lessOrEq(One)(lSize(lFilter(list)(predicate)))
}