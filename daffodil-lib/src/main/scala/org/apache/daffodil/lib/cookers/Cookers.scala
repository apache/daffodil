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

package org.apache.daffodil.lib.cookers

import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.lib.exceptions.ThrowsSDE

object TextStandardInfinityRepCooker
  extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object TextStandardNaNRepCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object TextStandardZeroRepCooker
  extends ListOfStringLiteralNoCharClass_NL_ES_EntitiesNoByteEntities()

class TextPadCharacterCookerBase
  extends SingleCharacterLiteralNoCharClassEntitiesWithByteEntities()

object TextNumberPadCharacterCooker extends TextPadCharacterCookerBase

object TextStringPadCharacterCooker extends TextPadCharacterCookerBase

object TextBooleanPadCharacterCooker extends TextPadCharacterCookerBase

object TextCalendarPadCharacterCooker extends TextPadCharacterCookerBase

object NilValueLiteralCharacterCooker
  extends SingleCharacterLiteralNoCharClassEntitiesWithByteEntities("nilValue")

object NilValueLogicalValueCooker
  extends NonEmptyListOfStringLiteralCharClass_ES_WithByteEntities(
    "nilValue"
  ) // Note. Same as LiteralValue Binary

object NilValueLiteralValueBinaryCooker
  extends NonEmptyListOfStringLiteralCharClass_ES_WithByteEntities("nilValue")

object NilValueLiteralValueTextCooker extends NonEmptyListOfStringLiteral("nilValue", true)

object NilValueRawListCooker
  extends ListOfStringLiteral("nilValue", true)
  with ListOfStringOneOrMoreLiteral

object EscapeCharacterCooker extends SingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object EscapeEscapeCharacterCooker
  extends SingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object EscapeBlockStartCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object EscapeBlockEndCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object ExtraEscapedCharactersCooker
  extends ListOfSingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object FillByteCooker extends SingleCharacterLiteralNoCharClassEntitiesWithByteEntities()

object InitiatorCooker extends DelimiterCooker("initiator")

object TerminatorCooker extends DelimiterCooker("terminator")

object TerminatorDelimitedCooker extends DelimiterCookerNoSoleES("terminator")

object SeparatorCooker extends DelimiterCookerNoES("separator")

object TextStandardDecimalSeparatorCooker
  extends ListOfSingleCharacterLiteralNoCharClassEntitiesNoByteEntities()
  with ListOfStringOneOrMoreLiteral

object TextStandardGroupingSeparatorCooker
  extends SingleCharacterLiteralNoCharClassEntitiesNoByteEntities()

object TextStandardExponentRepCooker extends StringLiteralNoCharClassEntitiesNoByteEntities()

object OutputNewLineCooker
  extends SingleCharacterLineEndingOrCRLF_NoCharClassEntitiesNoByteEntities()

object TextBooleanTrueRepCooker
  extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object TextBooleanFalseRepCooker
  extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object EncodingCooker extends UpperCaseToken()

object ChoiceDispatchKeyCooker extends StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object ChoiceBranchKeyCooker
  extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object RepValueCooker extends ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities()

object UpperCaseTokenCooker extends UpperCaseToken

object IntRangeCooker extends Converter[String, Seq[(JBigInt, JBigInt)]] {
  protected def convert(
    input: String,
    context: ThrowsSDE,
    forUnparse: Boolean
  ): Seq[(JBigInt, JBigInt)] = {
    def run(xs: Seq[String]): Seq[(JBigInt, JBigInt)] = {
      xs match {
        case Seq() => Seq()
        case a +: b +: rest => {
          val a2 = new JBigInt(a)
          val b2 = new JBigInt(b)
          if (a2.compareTo(b2) > 0) {
            context.SDE("min value (%s) is greater than max value (%s)", a2, b2)
          }
          (a2, b2) +: run(rest)
        }
      }
    }
    val asSeq = input.split("\\s+").toSeq.filter(!_.isEmpty)
    if (asSeq.length % 2 != 0) {
      context.SDE("Integer range sets must specify an even number of endpoints")
    }
    run(asSeq)
  }
}
