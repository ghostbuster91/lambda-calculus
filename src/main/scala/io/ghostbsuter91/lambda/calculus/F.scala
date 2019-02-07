package io.ghostbsuter91.lambda.calculus

trait F {
  def apply(f: F): F
}

object F {
  def Identity: F = x => x

  private def autocall: F = x => x(x)

  def Y: F = f => autocall(y => f(v => y(y)(v)))
}