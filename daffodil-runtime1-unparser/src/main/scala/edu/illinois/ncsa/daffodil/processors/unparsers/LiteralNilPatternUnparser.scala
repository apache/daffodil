package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.PrimUnparser

class LiteralNilPatternUnparser(erd: ElementRuntimeData, nilValue: String)
  extends PrimUnparser(erd) {

  Assert.invariant(erd.encodingInfo.isKnownEncoding)

  def unparse(state: UState) {
    val outStr = state.outStream
    val cs = erd.encodingInfo.knownEncodingCharset.charset
    outStr.encode(cs, nilValue)
  }

}