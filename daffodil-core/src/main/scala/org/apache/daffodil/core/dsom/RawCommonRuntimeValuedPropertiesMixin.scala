/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import org.apache.daffodil.lib.schema.annotation.props.PropertyMixin

trait RawCommonRuntimeValuedPropertiesMixin extends PropertyMixin {
  protected final lazy val optionByteOrderRaw =
    findPropertyOption("byteOrder", expressionAllowed = true)
  protected final lazy val byteOrderRaw = requireProperty(optionByteOrderRaw)
  protected final lazy val optionEncodingRaw =
    findPropertyOption("encoding", expressionAllowed = true)
  protected final lazy val encodingRaw = requireProperty(optionEncodingRaw)
  protected final lazy val optionOutputNewLineRaw =
    findPropertyOption("outputNewLine", expressionAllowed = true)
  protected final lazy val outputNewLineRaw = requireProperty(optionOutputNewLineRaw)

  protected final lazy val optionFillByteRaw = findPropertyOption("fillByte")
  protected final lazy val fillByteRaw = requireProperty(optionFillByteRaw)
}

trait RawDelimitedRuntimeValuedPropertiesMixin extends RawCommonRuntimeValuedPropertiesMixin {

  protected final lazy val optionInitiatorRaw =
    findPropertyOption("initiator", expressionAllowed = true)
  protected final lazy val initiatorRaw = requireProperty(optionInitiatorRaw)
  protected final lazy val optionTerminatorRaw =
    findPropertyOption("terminator", expressionAllowed = true)
  protected final lazy val terminatorRaw = requireProperty(optionTerminatorRaw)
  protected final lazy val optionChoiceLengthRaw = findPropertyOption("choiceLength")
  protected final lazy val choiceLengthRaw = requireProperty(optionChoiceLengthRaw)
}

trait RawElementRuntimeValuedPropertiesMixin
  extends RawDelimitedRuntimeValuedPropertiesMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin {

  // these are almost certainly not in scope, but on the local object
  // but not always. A type might define fixed length things for example.
  protected final lazy val optionLengthRaw =
    findPropertyOption("length", expressionAllowed = true)
  protected final lazy val lengthRaw = requireProperty(optionLengthRaw)
  protected final lazy val optionOccursCountRaw =
    findPropertyOption("occursCount", expressionAllowed = true)
  protected final lazy val occursCountRaw = requireProperty(optionOccursCountRaw)
}

trait RawSequenceRuntimeValuedPropertiesMixin extends RawDelimitedRuntimeValuedPropertiesMixin {

  protected final lazy val optionSeparatorRaw =
    findPropertyOption("separator", expressionAllowed = true)
  protected final lazy val separatorRaw = requireProperty(optionSeparatorRaw)
}

trait RawEscapeSchemeRuntimeValuedPropertiesMixin extends PropertyMixin {
  // package private because used in unit test
  final lazy val optionEscapeCharacterRaw =
    findPropertyOption("escapeCharacter", expressionAllowed = true)
  private[dsom] final lazy val escapeCharacterRaw = requireProperty(optionEscapeCharacterRaw)
  final lazy val optionEscapeEscapeCharacterRaw =
    findPropertyOption("escapeEscapeCharacter", expressionAllowed = true)
  protected final lazy val escapeEscapeCharacterRaw = requireProperty(
    optionEscapeEscapeCharacterRaw
  )
  protected final lazy val optionEscapeBlockStartRaw = findPropertyOption("escapeBlockStart")
  protected final lazy val escapeBlockStartRaw = requireProperty(optionEscapeBlockStartRaw)
  protected final lazy val optionEscapeBlockEndRaw = findPropertyOption("escapeBlockEnd")
  protected final lazy val escapeBlockEndRaw = requireProperty(optionEscapeBlockEndRaw)
  protected final lazy val optionExtraEscapedCharactersRaw =
    findPropertyOption("extraEscapedCharacters", expressionAllowed = true)
  protected final lazy val extraEscapedCharactersRaw = requireProperty(
    optionExtraEscapedCharactersRaw
  )

}

trait RawSimpleTypeRuntimeValuedPropertiesMixin extends RawCommonRuntimeValuedPropertiesMixin {

  protected final lazy val optionTextStandardDecimalSeparatorRaw =
    findPropertyOption("textStandardDecimalSeparator", expressionAllowed = true)
  protected final lazy val textStandardDecimalSeparatorRaw = requireProperty(
    optionTextStandardDecimalSeparatorRaw
  )
  protected final lazy val optionTextStandardGroupingSeparatorRaw =
    findPropertyOption("textStandardGroupingSeparator", expressionAllowed = true)
  protected final lazy val textStandardGroupingSeparatorRaw = requireProperty(
    optionTextStandardGroupingSeparatorRaw
  )

  protected final lazy val optionBinaryFloatRepRaw =
    findPropertyOption("binaryFloatRep", expressionAllowed = true)
  protected final lazy val binaryFloatRepRaw = requireProperty(optionBinaryFloatRepRaw)
  protected final lazy val optionTextBooleanTrueRepRaw =
    findPropertyOption("textBooleanTrueRep", expressionAllowed = true)
  protected final lazy val textBooleanTrueRepRaw = requireProperty(optionTextBooleanTrueRepRaw)
  protected final lazy val optionTextBooleanFalseRepRaw =
    findPropertyOption("textBooleanFalseRep", expressionAllowed = true)
  protected final lazy val textBooleanFalseRepRaw = requireProperty(
    optionTextBooleanFalseRepRaw
  )
  protected final lazy val optionCalendarLanguageRaw =
    findPropertyOption("calendarLanguage", expressionAllowed = true)
  protected final lazy val calendarLanguageRaw = requireProperty(optionCalendarLanguageRaw)
  protected final lazy val optionBinaryBooleanTrueRepRaw = findPropertyOption(
    "binaryBooleanTrueRep"
  )
  protected final lazy val binaryBooleanTrueRepRaw = requireProperty(
    optionBinaryBooleanTrueRepRaw
  )
  protected final lazy val optionBinaryBooleanFalseRepRaw = findPropertyOption(
    "binaryBooleanFalseRep"
  )
  protected final lazy val binaryBooleanFalseRepRaw = requireProperty(
    optionBinaryBooleanFalseRepRaw
  )

}
