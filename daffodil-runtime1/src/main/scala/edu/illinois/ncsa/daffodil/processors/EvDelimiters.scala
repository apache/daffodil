package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._

class InitiatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    InitiatorCooker,
    trd) {
  override lazy val runtimeDependencies = Nil
}

class TerminatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TerminatorCooker,
    trd) {
  override lazy val runtimeDependencies = Nil
}

class SeparatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    SeparatorCooker,
    trd) {
  override lazy val runtimeDependencies = Nil
}
