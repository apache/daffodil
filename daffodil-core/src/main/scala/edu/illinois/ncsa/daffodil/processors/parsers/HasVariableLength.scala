package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression

trait HasVariableLength { self: PrimParser =>
  def length: CompiledExpression

  def getLength(pstate: PState): (Long, PState) = {
    val (lengthAsAny: Any, newVMap) = length.evaluate(pstate)
    val l = convertToLong(lengthAsAny, pstate)
    val start = pstate.withVariables(newVMap)
    (l, start)
  }
}
