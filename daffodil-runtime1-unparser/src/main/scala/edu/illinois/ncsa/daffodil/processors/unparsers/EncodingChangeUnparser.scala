package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

class EncodingChangeUnparser(val termRuntimeData: TermRuntimeData)
  extends PrimUnparser(termRuntimeData) with TextUnparserRuntimeMixin {

  def unparse(state: UState): Unit = {
    setupEncoder(state, termRuntimeData)
  }

}