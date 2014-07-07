package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.dsom.ElementBase

trait HasVariableLength { self: PrimParser =>
  // Length is an expression
  private lazy val eb = self.context.asInstanceOf[ElementBase]
  val expr = eb.length
  val exprText = expr.prettyExpr

  def getLength(pstate: PState): (Long, PState) = {
    val R(lengthAsAny, newVMap) = expr.evaluate(pstate.parentElement, pstate.variableMap, pstate)
    val length = lengthAsAny.asInstanceOf[Long]
    val start = pstate.withVariables(newVMap)
    (length, start)
  }
}