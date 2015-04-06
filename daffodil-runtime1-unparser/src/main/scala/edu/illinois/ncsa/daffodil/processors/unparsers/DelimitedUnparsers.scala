package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.PrimUnparser

// FIXME: must do escaping of the string when it contains any in-scope delimiter
class StringDelimitedUnparser(
  erd: ElementRuntimeData,
  override val encodingInfo: EncodingInfo)
  extends PrimUnparser(erd)
  with RuntimeEncodingMixin {

  protected def theString(state: UState) = state.currentInfosetNode.get.asSimple.dataValueAsString

  def unparse(state: UState) {
    val valueString = theString(state)
    val outStream = state.outStream
    outStream.encode(dcharset.charset, valueString)
  }
}

class LiteralNilDelimitedEndOfDataUnparser(
  erd: ElementRuntimeData,
  nilValue: String,
  encInfo: EncodingInfo)
  extends StringDelimitedUnparser(erd, encInfo) {

  final override def theString(ignored: UState) = nilValue
}
