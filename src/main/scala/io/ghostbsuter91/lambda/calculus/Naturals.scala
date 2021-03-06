package io.ghostbsuter91.lambda.calculus

object Naturals {
  def Zero: F = _ => F.Identity

  def One: F = f => x => f(x)

  def Two: F = f => x => f(f(x))

  def Three: F = f => x => f(f(f(x)))

  def next: F = n => f => x => f(n(f)(x))

  def prev: F = n => f => x => n(g => h => h(g(f)))(_ => x)(u => u)

  def plus: F = m => n => f => x => m(f)(n(f)(x))

  def mult: F = m => n => f => m(n(f))

  def minus: F = m => n => n(prev)(m)

  def isZero: F = n => n(_ => Booleans.False)(Booleans.True)

  def lessOrEq: F = m => n => isZero(minus(m)(n))

  def isEq: F = m => n => Booleans.and(lessOrEq(m)(n))(lessOrEq(n)(m))

  def distance: F = m => n => lessOrEq(m)(n)(minus(n)(m))(minus(m)(n))

  private def powFactory: F = r => m => n => FlowControl.ifLambda(isZero(n))(_ => One)(_ => mult(m)(r(m)(prev(n))))

  def pow: F = F.Y(powFactory)
}
