package io.ghostbsuter91.lambda.calculus

object Booleans {

  def True: F =  x => y => x

  def False: F =  x => y => y

  def and: F = p => q => p(q)(p)

  def or: F =  p => q => p(p)(q)

  def neg: F =  p => p(False)(True)

  def xor: F =  p => q => or(and(p)(q))(and(neg(p))(neg(q)))
}




