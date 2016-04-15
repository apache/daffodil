package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import java.lang.{ Long => JLong }

class LengthEv(expr: CompiledExpression[JLong], rd: ElementRuntimeData)
  extends EvaluatableExpression[JLong](
    expr,
    rd)
  with InfosetCachedEvaluatable[JLong] {
  override lazy val runtimeDependencies = Nil
}

class OccursCountEv(expr: CompiledExpression[JLong], rd: ElementRuntimeData)
  extends EvaluatableExpression[JLong](
    expr,
    rd)
  with InfosetCachedEvaluatable[JLong] {
  override lazy val runtimeDependencies = Nil
}

class OutputNewLineEv(expr: CompiledExpression[String], rd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    OutputNewLineCooker,
    rd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
}
