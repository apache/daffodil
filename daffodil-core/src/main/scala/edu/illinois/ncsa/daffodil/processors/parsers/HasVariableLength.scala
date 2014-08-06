package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression

trait HasVariableLength { self: PrimParser =>
  def length: CompiledExpression

  def getLength(pstate: PState): (Long, PState) = {
    val R(lengthAsAny: Any, newVMap) = length.evaluate(pstate.parentElement, pstate.variableMap, pstate)
    val l = lengthAsAny.asInstanceOf[Long]
    val start = pstate.withVariables(newVMap)
    (l, start)
  }
}
