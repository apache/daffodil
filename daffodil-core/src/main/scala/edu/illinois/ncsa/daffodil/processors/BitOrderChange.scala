package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable

/**
 * Changes bit order to what the term specifies it is.
 */

class BitOrderChange(t: Term) extends Terminal(t, true) {
  val bitOrder = t.defaultBitOrder
  def parser = new BitOrderChangeParser(t.runtimeData, bitOrder)

}

class BitOrderChangeParser(t: RuntimeData, bitOrder: BitOrder) extends PrimParser(t) {
  def parse(pstate: PState): PState = {
    pstate.schemaDefinitionUnless(pstate.bitPos1b % 8 == 1, "Can only change dfdl:bitOrder on a byte boundary")
    pstate.withBitOrder(bitOrder)
  }
}
