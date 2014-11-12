package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

/**
 * Changes bit order to what the term specifies it is.
 */

class BitOrderChange(t: Term) extends Terminal(t, true) {
  val bitOrder = t.defaultBitOrder
  override lazy val parser = new BitOrderChangeParser(t.runtimeData, bitOrder)

}

