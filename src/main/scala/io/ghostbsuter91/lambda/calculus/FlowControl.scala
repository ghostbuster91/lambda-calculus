package io.ghostbsuter91.lambda.calculus

object FlowControl {

  def ifLambda: F =  c => t => f => c(t)(f)(F.Identity)
}
