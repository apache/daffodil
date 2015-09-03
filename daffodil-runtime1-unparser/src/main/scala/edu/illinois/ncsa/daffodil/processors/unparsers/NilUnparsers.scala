package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.util.MaybeChar

class LiteralNilOfSpecifiedLengthUnparser(
  unparsingPadChar: MaybeChar,
  justificationPad: TextJustificationType.Type,
  erd: ElementRuntimeData,
  outputNilValue: StringLiteralForUnparser)
  extends StringOfSpecifiedLengthUnparser(unparsingPadChar, justificationPad, erd,
    false // false meaning this is not for a string, but a literal nil
    ) {

  override protected def contentString(state: UState) = outputNilValue.evaluate(state)

}