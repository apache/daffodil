package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.processors.FillByteEv

class LiteralValueNilOfSpecifiedLengthUnparser(
  unparsingPadChar: MaybeChar,
  justificationPad: TextJustificationType.Type,
  erd: ElementRuntimeData,
  outputNilValue: StringLiteralForUnparser,
  isForPattern: Boolean,
  fillByteEv: FillByteEv)
  extends StringOfSpecifiedLengthUnparser(unparsingPadChar, justificationPad, erd,
    false, // false meaning this is not for a string, but a literal nil
    isForPattern,
    fillByteEv
    ) {

  override protected def contentString(state: UState) = outputNilValue.evaluate(state)

}

class LiteralCharacterNilOfSpecifiedLengthUnparser(
  erd: ElementRuntimeData,
  outputNilValue: StringLiteralForUnparser,
  isForPattern: Boolean,
  fillByteEv: FillByteEv)
  extends StringOfSpecifiedLengthUnparser(MaybeChar.Nope, TextJustificationType.Left, erd,
    false, // false meaning this is not for a string, but a literal nil
    isForPattern,
    fillByteEv
    ) {

  override protected def contentString(state: UState) = outputNilValue.evaluate(state)

  // This sets the pad character for this unparser to be the same as the
  // literal nil character. This way, we will unparse a single nil character,
  // and any left over space will be 'padded' with that same nil character.
  // This way, the entire specified length is filled with the nil character.
  // When unparsing literalCharacter, there really is no concept of padding.
  override def padChar(state: UState): MaybeChar = {
    val str = contentString(state)
    Assert.invariant(str.length == 1)
    MaybeChar(str(0))
  }
}
