package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.exceptions.Assert

class StringPatternMatchedUnparser(erd: ElementRuntimeData)
  extends PrimUnparser(erd) {

  Assert.invariant(erd.encodingInfo.isKnownEncoding)

  override def unparse(state: UState): Unit = {
    val valueString = state.currentInfosetNode.get.asSimple.dataValueAsString
    val outStream = state.outStream
    outStream.encode(erd.encodingInfo.knownEncodingCharset.charset, valueString)
  }
}