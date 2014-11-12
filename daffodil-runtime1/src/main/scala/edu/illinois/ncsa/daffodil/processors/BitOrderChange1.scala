package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

class BitOrderChangeParser(t: RuntimeData, bitOrder: BitOrder) extends PrimParser(t) {
  def parse(pstate: PState): PState = {
    pstate.schemaDefinitionUnless(pstate.bitPos1b % 8 == 1, "Can only change dfdl:bitOrder on a byte boundary")
    pstate.withBitOrder(bitOrder)
  }
}
