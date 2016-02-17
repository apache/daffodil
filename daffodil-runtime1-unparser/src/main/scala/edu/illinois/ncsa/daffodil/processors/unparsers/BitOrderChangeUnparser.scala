package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.CheckByteAndBitOrderEv
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

class BitOrderChangeUnparser(
  val termRuntimeData: TermRuntimeData,
  val bitOrder: BitOrder,
  val checkByteAndBitOrder: CheckByteAndBitOrderEv)
  extends PrimUnparserObject(termRuntimeData) {

  override lazy val runtimeDependencies = Seq(checkByteAndBitOrder)

  def unparse(state: UState): Unit = {
    checkByteAndBitOrder(state)
    termRuntimeData.schemaDefinitionUnless(state.bitPos1b % 8 == 1, "Can only change dfdl:bitOrder on a byte boundary")
    state.dataOutputStream.setBitOrder(bitOrder)
  }

}
