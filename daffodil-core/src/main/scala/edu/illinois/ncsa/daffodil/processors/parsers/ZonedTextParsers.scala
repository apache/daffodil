package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.exceptions.Assert

class ZonedTextNumberParser(gram: Gram, e: ElementBase)
  extends PrimParser(gram, e) {
  def parse(start: PState): PState = {
    Assert.notYetImplemented()
  }
}
