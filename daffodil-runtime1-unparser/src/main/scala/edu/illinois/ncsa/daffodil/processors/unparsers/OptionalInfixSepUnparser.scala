package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.processors.RuntimeData


class OptionalInfixSepUnparser(contextArg: RuntimeData,
  sepUnparser: Unparser)
  extends PrimUnparser(contextArg) {

  def unparse(state: UState): Unit = {
    if (state.arrayPos > 1) sepUnparser.unparse1(state, contextArg)
    else if (state.groupPos > 1) sepUnparser.unparse1(state, contextArg)
  }
}