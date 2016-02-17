package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

class EncodingChangeUnparser(val context: TermRuntimeData)
  extends PrimUnparser with TextUnparserRuntimeMixin {

  def runtimeDependencies = Seq(context.encodingInfo.encoderEv)

  def unparse(state: UState): Unit = {
    setupEncoder(state, context)
  }

}
