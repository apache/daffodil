/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyMixin

trait RawCommonRuntimeValuedPropertiesMixin
  extends PropertyMixin {
  protected final lazy val optionByteOrderRaw = findPropertyOption("byteOrder")
  protected final lazy val byteOrderRaw = requireProperty(optionByteOrderRaw)
  protected final lazy val optionEncodingRaw = findPropertyOption("encoding")
  protected final lazy val encodingRaw = requireProperty(optionEncodingRaw)
  protected final lazy val optionOutputNewLineRaw = findPropertyOption("outputNewLine")
  protected final lazy val outputNewLineRaw = requireProperty(optionOutputNewLineRaw)
}

trait RawDelimitedRuntimeValuedPropertiesMixin
  extends RawCommonRuntimeValuedPropertiesMixin {

  protected final lazy val optionInitiatorRaw = findPropertyOption("initiator")
  protected final lazy val initiatorRaw = requireProperty(optionInitiatorRaw)
  protected final lazy val optionTerminatorRaw = findPropertyOption("terminator")
  protected final lazy val terminatorRaw = requireProperty(optionTerminatorRaw)
}

trait RawElementRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin {

  // these are almost certainly not in scope, but on the local object
  // but not always. A type might define fixed length things for example.
  protected final lazy val optionLengthRaw = findPropertyOption("length")
  protected final lazy val lengthRaw = requireProperty(optionLengthRaw)
  protected final lazy val optionOccursCountRaw = findPropertyOption("occursCount")
  protected final lazy val occursCountRaw = requireProperty(optionOccursCountRaw)
}

trait RawSequenceRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin {

  protected final lazy val optionSeparatorRaw = findPropertyOption("separator")
  protected final lazy val separatorRaw = requireProperty(optionSeparatorRaw)
}

trait RawEscapeSchemeRuntimeValuedPropertiesMixin
  extends PropertyMixin {

  // package private because used in unit test
  final lazy val optionEscapeCharacterRaw = findPropertyOption("escapeCharacter")
  private[dsom] final lazy val escapeCharacterRaw = requireProperty(optionEscapeCharacterRaw)
  final lazy val optionEscapeEscapeCharacterRaw = findPropertyOption("escapeEscapeCharacter")
  protected final lazy val escapeEscapeCharacterRaw = requireProperty(optionEscapeEscapeCharacterRaw)
  protected final lazy val optionEscapeBlockStartRaw = findPropertyOption("escapeBlockStart")
  protected final lazy val escapeBlockStartRaw = requireProperty(optionEscapeBlockStartRaw)
  protected final lazy val optionEscapeBlockEndRaw = findPropertyOption("escapeBlockEnd")
  protected final lazy val escapeBlockEndRaw = requireProperty(optionEscapeBlockEndRaw)
  protected final lazy val optionExtraEscapedCharactersRaw = findPropertyOption("extraEscapedCharacters")
  protected final lazy val extraEscapedCharactersRaw = requireProperty(optionExtraEscapedCharactersRaw)

}

trait RawSimpleTypeRuntimeValuedPropertiesMixin
  extends RawCommonRuntimeValuedPropertiesMixin {

  protected final lazy val optionTextStandardDecimalSeparatorRaw = findPropertyOption("textStandardDecimalSeparator")
  protected final lazy val textStandardDecimalSeparatorRaw = requireProperty(optionTextStandardDecimalSeparatorRaw)
  protected final lazy val optionTextStandardGroupingSeparatorRaw = findPropertyOption("textStandardGroupingSeparator")
  protected final lazy val textStandardGroupingSeparatorRaw = requireProperty(optionTextStandardGroupingSeparatorRaw)
  protected final lazy val optionTextStandardExponentRepRaw = findPropertyOption("textStandardExponentRep")
  protected final lazy val textStandardExponentRepRaw = requireProperty(optionTextStandardExponentRepRaw)
  protected final lazy val optionBinaryFloatRepRaw = findPropertyOption("binaryFloatRep")
  protected final lazy val binaryFloatRepRaw = requireProperty(optionBinaryFloatRepRaw)
  protected final lazy val optionTextBooleanTrueRepRaw = findPropertyOption("textBooleanTrueRep")
  protected final lazy val textBooleanTrueRepRaw = requireProperty(optionTextBooleanTrueRepRaw)
  protected final lazy val optionTextBooleanFalseRepRaw = findPropertyOption("textBooleanFalseRep")
  protected final lazy val textBooleanFalseRepRaw = requireProperty(optionTextBooleanFalseRepRaw)
  protected final lazy val optionCalendarLanguageRaw = findPropertyOption("calendarLanguage")
  protected final lazy val calendarLanguageRaw = requireProperty(optionCalendarLanguageRaw)
  protected final lazy val optionBinaryBooleanTrueRepRaw = findPropertyOption("binaryBooleanTrueRep")
  protected final lazy val binaryBooleanTrueRepRaw = requireProperty(optionBinaryBooleanTrueRepRaw)
  protected final lazy val optionBinaryBooleanFalseRepRaw = findPropertyOption("binaryBooleanFalseRep")
  protected final lazy val binaryBooleanFalseRepRaw = requireProperty(optionBinaryBooleanFalseRepRaw)

}
