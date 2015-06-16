package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.BitOrderChangeMixin
import edu.illinois.ncsa.daffodil.processors.PrimUnparser

class BitOrderChangeUnparser(
  val runtimeData: RuntimeData,
  val bitOrder: BitOrder,
  val byteOrder: CompiledExpression)
  extends PrimUnparser(runtimeData) with BitOrderChangeMixin {

  def unparse(state: UState): Unit = {
    checkByteAndBitOrder(state)
    state.dataOutputStream.setBitOrder(bitOrder)
  }

}