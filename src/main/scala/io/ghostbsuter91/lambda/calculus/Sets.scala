package io.ghostbsuter91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.Booleans._
import io.ghostbsuter91.lambda.calculus.Naturals._

object Sets {

  val _value: F = Lists._value
  val _prev: F = Lists._prev
  val _isEmpty: F = Lists._isEmpty

  private val addUnsafe: F = list => elem => p => p(elem)(list)(False)

  val add: F = list => elem => comp => FlowControl.ifLambda(sContains(list)(comp(elem)))(_ => list)(_ => addUnsafe(list)(elem))

  val emptySet: F = Lists.emptyList

  def fold: F = Lists.fold

  def sSize: F = set => fold(set)(acc => _ => plus(acc)(One))(Zero)

  def sSum: F = set => fold(set)(acc => item => plus(acc)(item))(Zero)

  def sAdd: F = set1 => set2 => comp => fold(set2)(acc => item => add(acc)(item)(comp))(set1)

  def sFilter: F = set => predicate => fold(set)(acc => item => FlowControl.ifLambda(predicate(item))(_ => addUnsafe(acc)(item))(_ => acc))(emptySet)

  def sContains: F = set => predicate => lessOrEq(One)(sSize(sFilter(set)(predicate)))

  def sEq: F = set1 => set2 => comp => and(isEq(sSize(set1))(sSize(set2)))(fold(set1)(acc => item => and(acc)(sContains(set2)(comp(item))))(True))

  def sFlatten: F = set => comp => fold(set)(acc => item => sAdd(acc)(item)(comp))(emptySet)

  def sIntersect: F = set1 => set2 => comp => sAdd(sFilter(set1)(item => sContains(set2)(comp(item))))(sFilter(set2)(item => sContains(set1)(comp(item))))(comp)

  private def map: F = set => f => comp => adder => fold(set)(acc => item => adder(acc)(f(item))(comp))(emptySet)

  def sMap: F = set => f => comp => map(set)(f)(comp)(add)

  def sFlatMap: F = set => f => comp => map(set)(f)(comp)(sAdd)

  def sFlatMapUsingFlatten: F = set => f => comp => sFlatten(sMap(set)(f)(s1 => s2 => sEq(s1)(s2)(comp)))(comp)
}