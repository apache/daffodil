package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.CheckBitOrderAndCharsetEv

class EncodingChangeUnparser(val context: TermRuntimeData, checkBitOrderAndCharset: CheckBitOrderAndCharsetEv)
  extends PrimUnparser with TextUnparserRuntimeMixin {

  lazy val runtimeDependencies = Seq(context.encodingInfo.charsetEv, checkBitOrderAndCharset)

  def unparse(state: UState): Unit = {
    checkBitOrderAndCharset(state)
    setupEncoder(state, context)
  }

}
