package io.ghostbsuter91.lambda.calculus

object Numbers {

  val _value: F = x => _ => x
  val _isPositive: F = _ => y => y

  val Zero: F = toNumber(Naturals.Zero)
  val One: F = toNumber(Naturals.One)
  val Two: F = toNumber(Naturals.Two)
  val Three: F = toNumber(Naturals.Three)
  val nOne: F = negative(One)
  val nTwo: F = negative(Two)
  val nThree: F = negative(Three)

  def negative: F = m => isZero(m)(Zero)(n => n(m(_value))(Booleans.neg(m(_isPositive))))

  def isZero: F = n => Naturals.isZero(n(_value))

  def toNumber: F = m => n => n(m)(Booleans.True)

  def next: F = n => n(_isPositive)(toNumber(Naturals.next(n(_value))))(negative(toNumber(Naturals.prev(n(_value)))))

  def plus: F = m => n => Booleans.xor(n(_isPositive))(m(_isPositive))(k => k(Naturals.plus(n(_value))(m(_value)))(n(_isPositive)))(k => k(Naturals.distance(m(_value))(n(_value)))(signOfGreater(n)(m)))

  def minus: F = m => n => plus(m)(negative(n))

  private def signOfGreater: F = m => n => isAbsEq(m)(n)(Booleans.True)(Naturals.lessOrEq(m(_value))(n(_value))(n(_isPositive))(m(_isPositive)))

  def prev: F = n => minus(n)(One)

  def isEq: F = m => n => Booleans.and(Naturals.isEq(m(_value))(n(_value)))(Booleans.xor(m(_isPositive))(n(_isPositive)))

  def isAbsEq: F = m => n => Naturals.isEq(m(_value))(n(_value))

  def mult: F = m => n => k => k(Naturals.mult(m(_value))(n(_value)))(Booleans.or(Booleans.or(isZero(m))(isZero(n)))(Booleans.xor(m(_isPositive))(n(_isPositive))))
}
