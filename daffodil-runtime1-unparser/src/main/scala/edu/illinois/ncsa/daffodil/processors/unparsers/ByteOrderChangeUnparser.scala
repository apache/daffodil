package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors._

class ByteOrderChangeUnparser(val elementRuntimeData: ElementRuntimeData, byteOrdEv: ByteOrderEv, check: CheckByteAndBitOrderEv)
  extends PrimUnparser with BinaryParserUnparserRuntimeMixin {

  override def context = elementRuntimeData

  override lazy val runtimeDependencies = Seq(byteOrdEv, check)

  def unparse(state: UState): Unit = {
    check(state)
    setupByteOrder(state, elementRuntimeData, byteOrdEv)
  }

}
