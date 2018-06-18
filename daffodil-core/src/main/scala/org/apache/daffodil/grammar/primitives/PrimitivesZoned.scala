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

import org.apache.daffodil.dsom._
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import com.ibm.icu.text.DecimalFormat
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.unparsers.ConvertZonedNumberUnparser
import org.apache.daffodil.processors.unparsers.ConvertZonedCombinatorUnparser
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.parsers.ConvertZonedNumberParser
import org.apache.daffodil.processors.parsers.ConvertZonedCombinatorParser
import org.apache.daffodil.processors.parsers.ConvertTextByteParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextDecimalParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextIntParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextIntegerParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextLongParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextNonNegativeIntegerParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextNumberParserUnparserHelperBase
import org.apache.daffodil.processors.parsers.ConvertTextShortParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedByteParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedLongParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedShortParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedIntParserUnparserHelper
import org.apache.daffodil.processors.parsers.NumberFormatFactoryBase
import org.apache.daffodil.processors.parsers.NumberFormatFactoryStatic
import org.apache.daffodil.schema.annotation.props.gen.TextNumberCheckPolicy
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeDouble

import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }

case class ConvertZonedCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  override lazy val parser = new ConvertZonedCombinatorParser(e.termRuntimeData, value.parser, converter.parser)

  override lazy val unparser = new ConvertZonedCombinatorUnparser(e.termRuntimeData, value.unparser, converter.unparser)
}

abstract class ConvertZonedNumberPrim[S](e: ElementBase)
  extends Terminal(e, true) {

  def helper: ConvertTextNumberParserUnparserHelperBase[S]

  def numFormatFactory: NumberFormatFactoryBase[S] = {
    val h = helper

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

    val nff = new NumberFormatFactoryStatic[S](
      e.termRuntimeData, h,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope,
      Maybe.Nope,
      e.textNumberCheckPolicy,
      zonedPattern,
      e.textNumberRounding,
      roundingMode,
      roundingIncrement)
    nff
  }

  lazy val parser: Parser = new ConvertZonedNumberParser[S](helper, e.textNumberPattern, numFormatFactory, e.textZonedSignStyle, e.elementRuntimeData)

  override lazy val unparser: Unparser = new ConvertZonedNumberUnparser[S](helper, e.textNumberPattern, e.textZonedSignStyle, e.elementRuntimeData)
}

case class ConvertZonedIntegerPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigInt](e) {
  val helper = new ConvertTextIntegerParserUnparserHelper[JBigInt](List(), false)
}

case class ConvertZonedDecimalPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigDecimal](e) {
  val helper = new ConvertTextDecimalParserUnparserHelper[JBigDecimal](List(), false)
}

case class ConvertZonedNonNegativeIntegerPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigInt](e) {
  val helper = new ConvertTextNonNegativeIntegerParserUnparserHelper[JBigDecimal](List(), false)
}

case class ConvertZonedLongPrim(e: ElementBase) extends ConvertZonedNumberPrim[Long](e) {
  val helper = new ConvertTextLongParserUnparserHelper[Long](List(), false)
}

case class ConvertZonedIntPrim(e: ElementBase) extends ConvertZonedNumberPrim[Int](e) {
  val helper = new ConvertTextIntParserUnparserHelper[Int](List(), false)
}

case class ConvertZonedShortPrim(e: ElementBase) extends ConvertZonedNumberPrim[Short](e) {
  val helper = new ConvertTextShortParserUnparserHelper[Short](List(), false)
}

case class ConvertZonedBytePrim(e: ElementBase) extends ConvertZonedNumberPrim[Byte](e) {
  val helper = new ConvertTextByteParserUnparserHelper[Byte](List(), false)
}

case class ConvertZonedUnsignedLongPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigInt](e) {
  val helper = new ConvertTextUnsignedLongParserUnparserHelper[JBigInt](List(), false)
}

case class ConvertZonedUnsignedIntPrim(e: ElementBase) extends ConvertZonedNumberPrim[Long](e) {
  val helper = ConvertTextUnsignedIntParserUnparserHelper[Long](List(), false)
}

case class ConvertZonedUnsignedShortPrim(e: ElementBase) extends ConvertZonedNumberPrim[Int](e) {
  val helper = new ConvertTextUnsignedShortParserUnparserHelper[Int](List(), false)
}

case class ConvertZonedUnsignedBytePrim(e: ElementBase) extends ConvertZonedNumberPrim[Short](e) {
  val helper = new ConvertTextUnsignedByteParserUnparserHelper[Short](List(), false)
}
