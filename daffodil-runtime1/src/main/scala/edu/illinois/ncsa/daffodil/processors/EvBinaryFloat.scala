package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom._

class BinaryFloatRepEv(expr: CompiledExpression[String], erd: ElementRuntimeData)
  extends EvaluatableConvertedExpression[String, BinaryFloatRep](
    expr,
    BinaryFloatRep,
    erd) {
  override lazy val runtimeDependencies = Nil

}
