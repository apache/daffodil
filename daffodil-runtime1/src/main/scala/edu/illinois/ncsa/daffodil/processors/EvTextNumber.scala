package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._

class TextStandardDecimalSeparatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextStandardDecimalSeparatorCooker,
    trd)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Nil
}

class TextStandardGroupingSeparatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardGroupingSeparatorCooker,
    trd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
}

class TextStandardExponentRepEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardExponentRepCooker,
    trd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
}

class TextBooleanTrueRepEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextBooleanTrueRepCooker,
    trd)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Nil
}

class TextBooleanFalseRepEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextBooleanFalseRepCooker,
    trd)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Nil
}
