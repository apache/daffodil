package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData

class OptionalInfixSepUnparser(contextArg: RuntimeData,
  sepUnparser: Unparser)
  extends UnparserObject(contextArg) {

  override lazy val childProcessors = List(sepUnparser)

  def unparse(state: UState): Unit = {
    if (state.arrayPos > 1) sepUnparser.unparse1(state, contextArg)
    else if (state.groupPos > 1) sepUnparser.unparse1(state, contextArg)
  }
}
