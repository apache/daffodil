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

package org.apache.daffodil.grammar.primitives


import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom._
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.TextNumberFormatEv
import org.apache.daffodil.processors.parsers.ConvertZonedCombinatorParser
import org.apache.daffodil.processors.parsers.ConvertZonedNumberParser
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.unparsers.ConvertZonedCombinatorUnparser
import org.apache.daffodil.processors.unparsers.ConvertZonedNumberUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.schema.annotation.props.gen.TextNumberCheckPolicy
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.util.DecimalUtils.OverpunchLocation
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeDouble

case class ConvertZonedCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  override lazy val parser = new ConvertZonedCombinatorParser(e.termRuntimeData, value.parser, converter.parser)

  override lazy val unparser = new ConvertZonedCombinatorUnparser(e.termRuntimeData, value.unparser, converter.unparser)
}

case class ConvertZonedNumberPrim(e: ElementBase)
  extends Terminal(e, true)
  with ConvertTextNumberMixin {

  final override protected lazy val textDecimalVirtualPointFromPattern: Int = {
    TextNumberPatternUtils.textDecimalVirtualPointForZoned(patternStripped).getOrElse {
      e.SDE(
        s"""The dfdl:textNumberPattern '%s' contains 'V' (virtual decimal point).
           |Other than the leading or trailing '+' sign indicator,
           |it can contain only digits 0-9.""".stripMargin('|'),
        pattern)
    }
  }

  lazy val textNumberFormatEv: TextNumberFormatEv = {

    if (patternStripped.contains("@")) {
      e.SDE("The '@' symbol may not be used in textNumberPattern for textNumberRep='zoned'")
    }

    if (patternStripped.contains("E")) {
      e.SDE("The 'E' symbol may not be used in textNumberPattern for textNumberRep='zoned'")
    }

    e.schemaDefinitionWhen(patternStripped.contains(";"),
      "Negative patterns may not be used in textNumberPattern for textNumberRep='zoned'")

    e.primType match {
      case PrimType.Double | PrimType.Float => e.SDE("textNumberRep='zoned' does not support Doubles/Floats")
      case PrimType.UnsignedLong | PrimType.UnsignedInt | PrimType.UnsignedShort | PrimType.UnsignedByte => {
        if (e.textNumberCheckPolicy == TextNumberCheckPolicy.Lax) {
          if ((patternStripped(0) != '+') && (patternStripped(patternStripped.length - 1) != '+'))
            e.SDE("textNumberPattern must have '+' at the beginning or the end of the pattern when textNumberRep='zoned' and textNumberPolicy='lax' for unsigned numbers")
        }
      }
      case _ => {
        if ((patternStripped(0) != '+') && (patternStripped(patternStripped.length - 1) != '+'))
          e.SDE("textNumberPattern must have '+' at the beginning or the end of the pattern when textNumberRep='zoned' for signed numbers")
      }
    }

    if ((patternStripped(0) == '+') && (patternStripped(patternStripped.length - 1) == '+'))
      e.SDE("The textNumberPattern may either begin or end with a '+', not both.")

    if (textDecimalVirtualPoint > 0) {
      e.primType match {
        case PrimType.Decimal => // ok
        case _ => e.SDE(
          """The dfdl:textNumberPattern has a virtual decimal point 'V' and dfdl:textNumberRep='zoned'.
            | The type must be xs:decimal, but was: %s.""".stripMargin, e.primType.globalQName.toPrettyString)
      }
    }

    checkPatternWithICU(e)

    /* Need to remove the '+' from the number pattern as '+' is only
    *  used to indicate which digit of the number is overpunched when
    *  dealing with zoned decimal formats. If the '+' is not removed
    *  the underlying ICU library will attempt to use it as an indicator for exponent
    *  numbers, which will be wrong for zoned number parsing.
    */
    val zonedPattern = runtimePattern.replace("+", "")

    val (roundingIncrement, roundingMode) =
      e.textNumberRounding match {
        case TextNumberRounding.Explicit => (MaybeDouble(e.textNumberRoundingIncrement), One(e.textNumberRoundingMode))
        case TextNumberRounding.Pattern => (MaybeDouble.Nope, Nope)
      }

    val ev = new TextNumberFormatEv(
      e.tci,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope,
      e.textNumberCheckPolicy,
      zonedPattern,
      e.textNumberRounding,
      roundingMode,
      roundingIncrement,
      Nil,
      isInt = true,
      e.primType)
    ev.compile(tunable)
    ev
  }

  val opindex = e.textNumberPattern.indexOf('+')
  val opl = {
    if (opindex == 0)
      OverpunchLocation.Start
    else if (opindex == e.textNumberPattern.length - 1)
      OverpunchLocation.End
    else
      OverpunchLocation.None
  }

  lazy val parser: Parser =
    new ConvertZonedNumberParser(opl, textNumberFormatEv, e.textZonedSignStyle, e.elementRuntimeData,
      textDecimalVirtualPoint)

  override lazy val unparser: Unparser =
    new ConvertZonedNumberUnparser(opl, e.textZonedSignStyle, e.elementRuntimeData,
      textDecimalVirtualPoint)
}
