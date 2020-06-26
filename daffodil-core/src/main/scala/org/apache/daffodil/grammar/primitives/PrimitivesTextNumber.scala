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


import org.apache.daffodil.cookers.EntityReplacer
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom._
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.Delimiter
import org.apache.daffodil.processors.parsers.ConvertTextCombinatorParser
import org.apache.daffodil.processors.parsers.ConvertTextNumberParser
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.TextNumberFormatEv
import org.apache.daffodil.processors.unparsers.ConvertTextCombinatorUnparser
import org.apache.daffodil.processors.unparsers.ConvertTextNumberUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeDouble

case class ConvertTextCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  lazy val parser = new ConvertTextCombinatorParser(e.termRuntimeData, value.parser, converter.parser)

  override lazy val unparser = new ConvertTextCombinatorUnparser(e.termRuntimeData, value.unparser, converter.unparser)
}

case class ConvertTextNumberPrim(e: ElementBase)
  extends Terminal(e, true) {

  val zeroRepsRaw = e.textStandardZeroRep.filter { _ != "" }
  val zeroRepsRegex = zeroRepsRaw.map { zr =>
    val d = new Delimiter()
    d.compileDelimiter(zr, e.ignoreCaseBool)
    // add '^' and '$' to require the regular expression to match the entire
    // string as a zero rep instead of just part of it
    val ignoreCaseStr = if (e.ignoreCaseBool) "(?i)" else ""
    val regex = (ignoreCaseStr + "^" + d.delimRegExParseDelim + "$").r
    regex
  }
  val zeroRepUnparse: Maybe[String] = zeroRepsRaw.headOption.map { zr =>
    EntityReplacer { _.replaceForUnparse(zr) }
  }

  val textNumberFormatEv: TextNumberFormatEv = {
    val (pattern, patternStripped) = {
      val p = e.textNumberPattern

      if (p.startsWith(";")) {
        e.SDE("Invalid textNumberPattern: The postive number pattern is mandatory")
      }

      val noEscapedTicksRegex = """''""".r
      val patternNoEscapedTicks = noEscapedTicksRegex.replaceAllIn(p, "")
      val noQuotedRegex = """'[^']+'""".r
      val patternNoQuoted = noQuotedRegex.replaceAllIn(patternNoEscapedTicks, "")

      if (patternNoQuoted.contains("V")) {
        e.notYetImplemented("textNumberPattern with V symbol")
      }

      if (patternNoQuoted.contains("P")) {
        e.notYetImplemented("textNumberPattern with P symbol")
      }

      // Load the pattern to make sure it is valid
      try {
        new DecimalFormat(p)
      } catch {
        case ex: IllegalArgumentException => e.SDE("Invalid textNumberPattern: " + ex.getMessage())
      }

      (p, patternNoQuoted)
    }

    val (roundingIncrement: MaybeDouble, roundingMode) =
      e.textNumberRounding match {
        case TextNumberRounding.Explicit => (MaybeDouble(e.textNumberRoundingIncrement), One(e.textNumberRoundingMode))
        case TextNumberRounding.Pattern => (MaybeDouble.Nope, Nope)
      }

    val (infRep, nanRep) = e.primType match {
      case PrimType.Double | PrimType.Float => (One(e.textStandardInfinityRep), One(e.textStandardNaNRep))
      case _ => (Nope, Nope)
    }

    val isInt = e.primType match {
      case PrimType.Double | PrimType.Float | PrimType.Decimal => false
      case _ => true
    }

    // If the pattern contains any of these characters, we need to set both
    // group and decimal separators, even if the pattern doesn't contain the
    // associated character. This is because even when the pattern does not
    // contain the grouping/decimal separators, ICU stills seems to take the
    // separators into account. And since ICU provides defaut values based on
    // locales, not setting them can cause subtle locale related bugs. We must
    // also require the separators if the prim type is not an integer type,
    // since ICU will use them even if the pattern does not specify them.
    val requireDecGroupSeps =
      patternStripped.contains(",") || patternStripped.contains(".") ||
      patternStripped.contains("E") || patternStripped.contains("@") ||
      !isInt

    val decSep =
      if (requireDecGroupSeps) {
        One(e.textStandardDecimalSeparatorEv)
      } else {
        Nope
      }

    val groupSep =
      if (requireDecGroupSeps) {
        One(e.textStandardGroupingSeparatorEv)
      } else {
        Nope
      }

    val ev = new TextNumberFormatEv(
      e.tci,
      decSep,
      groupSep,
      One(e.textStandardExponentRepEv),
      infRep,
      nanRep,
      e.textNumberCheckPolicy,
      pattern,
      e.textNumberRounding,
      roundingMode,
      roundingIncrement,
      zeroRepsRaw,
      isInt,
      e.primType)
    ev.compile(tunable)
    ev
  }

  lazy val parser: Parser = new ConvertTextNumberParser(textNumberFormatEv, zeroRepsRegex, e.elementRuntimeData)

  override lazy val unparser: Unparser = new ConvertTextNumberUnparser(textNumberFormatEv, zeroRepUnparse, e.elementRuntimeData)
}
