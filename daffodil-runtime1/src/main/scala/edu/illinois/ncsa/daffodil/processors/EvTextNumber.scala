package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._

class TextStandardDecimalSeparatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextStandardDecimalSeparatorCooker,
    trd) {
  override def runtimeDependencies = Nil
}

class TextStandardGroupingSeparatorEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardGroupingSeparatorCooker,
    trd) {
  override def runtimeDependencies = Nil
}

class TextStandardExponentRepEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardExponentRepCooker,
    trd) {
  override def runtimeDependencies = Nil
}

class TextBooleanTrueRepEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextBooleanTrueRepCooker,
    trd) {
  override def runtimeDependencies = Nil
}

class TextBooleanFalseRepEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextBooleanFalseRepCooker,
    trd) {
  override def runtimeDependencies = Nil
}
