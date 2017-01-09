/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

object TextStandardInfinityRepCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object TextStandardNaNRepCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object TextStandardZeroRepCooker extends ListOfStringLiteralNoCharClass_NL_ES_EntitiesNoByteEntities()

class TextPadCharacterCookerBase extends SingleCharacterLiteralNoCharClassEntitiesWithByteEntities()

object TextNumberPadCharacterCooker extends TextPadCharacterCookerBase

object TextStringPadCharacterCooker extends TextPadCharacterCookerBase

object TextBooleanPadCharacterCooker extends TextPadCharacterCookerBase

object TextCalendarPadCharacterCooker extends TextPadCharacterCookerBase

object NilValueLiteralCharacterCooker extends SingleCharacterLiteralNoCharClassEntitiesWithByteEntities("nilValue")

object NilValueLogicalValueCooker extends NonEmptyListOfStringLiteralCharClass_ES_WithByteEntities("nilValue") // Note. Same as LiteralValue Binary

object NilValueLiteralValueBinaryCooker extends NonEmptyListOfStringLiteralCharClass_ES_WithByteEntities("nilValue")

object NilValueLiteralValueTextCooker extends NonEmptyListOfStringLiteral("nilValue", true)

object NilValueRawListCooker extends ListOfStringLiteral("nilValue", false)

object EscapeCharacterCooker extends SingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object EscapeEscapeCharacterCooker extends SingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object EscapeBlockStartCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object EscapeBlockEndCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object ExtraEscapedCharactersCooker extends ListOfSingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object FillByteCooker extends SingleCharacterLiteralNoCharClassEntitiesWithByteEntities()

object InitiatorCooker extends DelimiterCooker()

object TerminatorCooker extends DelimiterCooker()

object TerminatorCookerNoES extends DelimiterCookerNoES("terminator")

object SeparatorCooker extends DelimiterCookerNoES(null)

object TextStandardDecimalSeparatorCooker extends ListOfSingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object TextStandardGroupingSeparatorCooker extends SingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object TextStandardExponentRepCooker extends StringLiteralNoCharClassEntitiesNoByteEntities()

object OutputNewLineCooker extends SingleCharacterLineEndingOrCRLF_NoCharClassEntitiesNoByteEntities()

object TextBooleanTrueRepCooker extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object TextBooleanFalseRepCooker extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object EncodingCooker extends UpperCaseToken()

object ChoiceDispatchKeyCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object ChoiceBranchKeyCooker extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()
