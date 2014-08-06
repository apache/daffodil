package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData

class ZonedTextNumberParser(e: ElementRuntimeData)
  extends PrimParser(e) {
  def parse(start: PState): PState = {
    e.notYetImplemented("Zoned Numbers")
  }
}
