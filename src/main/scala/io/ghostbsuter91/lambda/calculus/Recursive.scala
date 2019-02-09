package io.ghostbsuter91.lambda.calculus

import io.ghostbsuter91.lambda.calculus.F._
import io.ghostbsuter91.lambda.calculus.Naturals._

object Recursive {

  private def factorialFactory: F = r => n => FlowControl.ifLambda(isZero(n))(_ => One)(_ => mult(n)(r(prev(n))))

  def factorial: F = Y(factorialFactory)

  private def fibonacciFactory: F = r => n => FlowControl.ifLambda(lessOrEq(n)(One))(_ => n)(_ => plus(r(prev(n)))(r(prev(prev(n)))))

  def fibonacci: F = Y(fibonacciFactory)
}
