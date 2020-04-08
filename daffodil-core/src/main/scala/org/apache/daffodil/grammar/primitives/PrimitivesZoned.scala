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


import com.ibm.icu.text.DecimalFormat

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
  extends Terminal(e, true) {

  val textNumberFormatEv: TextNumberFormatEv = {
    val pattern = {
      val p = e.textNumberPattern

      val noEscapedTicksRegex = """''""".r
      val patternNoEscapedTicks = noEscapedTicksRegex.replaceAllIn(p, "")
      val noQuotedRegex = """'[^']+'""".r
      val patternNoQuoted = noQuotedRegex.replaceAllIn(patternNoEscapedTicks, "")
      val zonedPatternRegex = "\\+?#*[0-9]+\\+?".r

      if (patternNoQuoted.contains("V")) {
        e.notYetImplemented("textNumberPattern with V symbol")
      }

      if (patternNoQuoted.contains("P")) {
        e.notYetImplemented("textNumberPattern with P symbol")
      }

      if (patternNoQuoted.contains("@")) {
        e.SDE("The '@' symbol may not be used in textNumberPattern for textNumberRep='zoned'")
      }

      if (patternNoQuoted.contains(";")) {
        e.SDE("Negative patterns may not be used in textNumberPattern for textNumberRep='zoned'")
      }

      e.primType match {
        case PrimType.Double | PrimType.Float => e.SDE("textZonedFormat='zoned' does not support Doubles/Floats")
        case PrimType.UnsignedLong | PrimType.UnsignedInt | PrimType.UnsignedShort | PrimType.UnsignedByte => {
          if (e.textNumberCheckPolicy == TextNumberCheckPolicy.Lax) {
            if ((patternNoQuoted(0) != '+') && (patternNoQuoted(patternNoQuoted.length - 1) != '+'))
              e.SDE("textNumberPattern must have '+' at the beginning or the end of the pattern when textZonedFormat='zoned' and textNumberPolicy='lax' for unsigned numbers")
          }
        }
        case _ => {
          if ((patternNoQuoted(0) != '+') && (patternNoQuoted(patternNoQuoted.length - 1) != '+'))
            e.SDE("textNumberPattern must have '+' at the beginning or the end of the pattern when textZonedFormat='zoned' for signed numbers")
        }
      }

      if ((patternNoQuoted(0) == '+') && (patternNoQuoted(patternNoQuoted.length - 1) == '+'))
        e.SDE("The textNumberPattern may either begin or end with a '+', not both.")

      if (!zonedPatternRegex.pattern.matcher(patternNoQuoted).matches)
        e.SDE("Invalid characters used in textNubmerPattern for zoned number. Only the following characters may be used with zoned numbers: '+', 'V', 'P', '0-9', and '#'")

      // Load the pattern to make sure it is valid
      try {
        new DecimalFormat(p.replace("+", ""))
      } catch {
        case ex: IllegalArgumentException => e.SDE("Invalid textNumberPattern: " + ex.getMessage())
      }

      p
    }

    /* Need to remove the '+' from the number pattern as '+' is only
    *  used to indicate which digit of the number is overpunched when
    *  dealing with zoned decimal formats. If the '+' is not removed
    *  DecimalFormat will attempt to use it as an indicator for exponent
    *  numbers, which will most likely not match the zoned number being
    *  parsed */
    val zonedPattern = pattern.replace("+", "")

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

  lazy val parser: Parser = new ConvertZonedNumberParser(opl, textNumberFormatEv, e.textZonedSignStyle, e.elementRuntimeData)

  override lazy val unparser: Unparser = new ConvertZonedNumberUnparser(opl, e.textZonedSignStyle, e.elementRuntimeData)
}
