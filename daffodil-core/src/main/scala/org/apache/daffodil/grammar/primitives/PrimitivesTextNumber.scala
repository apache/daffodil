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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.dsom._
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import com.ibm.icu.text.DecimalFormat
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.unparsers.ConvertTextNumberUnparser
import org.apache.daffodil.processors.unparsers.ConvertTextCombinatorUnparser
import org.apache.daffodil.util.MaybeDouble
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import org.apache.daffodil.processors.parsers.ConvertTextByteParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextCombinatorParser
import org.apache.daffodil.processors.parsers.ConvertTextDecimalParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextDoubleParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextFloatParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextIntParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextIntegerParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextLongParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextNonNegativeIntegerParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextNumberParser
import org.apache.daffodil.processors.parsers.ConvertTextNumberParserUnparserHelperBase
import org.apache.daffodil.processors.parsers.ConvertTextShortParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedByteParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedLongParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedShortParserUnparserHelper
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.parsers.NumberFormatFactoryBase
import org.apache.daffodil.processors.parsers.NumberFormatFactoryDynamic
import org.apache.daffodil.processors.parsers.NumberFormatFactoryStatic
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import org.apache.daffodil.processors.parsers.ConvertTextUnsignedIntParserUnparserHelper
import org.apache.daffodil.processors.parsers.Parser

case class ConvertTextCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  lazy val parser = new ConvertTextCombinatorParser(e.runtimeData, value.parser, converter.parser)

  override lazy val unparser = new ConvertTextCombinatorUnparser(e.runtimeData, value.unparser, converter.unparser)
}

abstract class ConvertTextNumberPrim[S](e: ElementBase)
  extends Terminal(e, true) {

  def helper: ConvertTextNumberParserUnparserHelperBase[S]

  def numFormatFactory: NumberFormatFactoryBase[S] = {
    val h = helper

    val (pattern, patternStripped) = {
      val p = e.textNumberPattern

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

    val (infRep, nanRep) =
      if (h.allowInfNaN) {
        (One(e.textStandardInfinityRep), One(e.textStandardNaNRep))
      } else {
        (Nope, Nope)
      }

    val decSep: Maybe[Evaluatable[List[String]]] =
      if (!h.isInt && (patternStripped.contains(".") ||
        patternStripped.contains("E") ||
        patternStripped.contains("@"))) {
        One(e.textStandardDecimalSeparatorEv)
      } else {
        Nope
      }

    val groupSep =
      if (patternStripped.contains(",")) {
        One(e.textStandardGroupingSeparatorEv)
      } else {
        Nope
      }

    val isConstant = ((decSep.isEmpty || decSep.get.isConstant) &&
      (groupSep.isEmpty || groupSep.get.isConstant) &&
      e.textStandardExponentRepEv.isConstant)

    val nff = if (isConstant) {
      new NumberFormatFactoryStatic[S](e.runtimeData, h,
        decSep,
        groupSep,
        e.textStandardExponentRepEv,
        infRep,
        nanRep,
        e.textNumberCheckPolicy,
        pattern,
        e.textNumberRounding,
        roundingMode,
        roundingIncrement)
    } else {
      new NumberFormatFactoryDynamic[S](e.runtimeData, h,
        decSep,
        groupSep,
        e.textStandardExponentRepEv,
        infRep,
        nanRep,
        e.textNumberCheckPolicy,
        pattern,
        e.textNumberRounding,
        roundingMode,
        roundingIncrement)
    }
    nff
  }

  lazy val parser: Parser = new ConvertTextNumberParser[S](helper, numFormatFactory, e.elementRuntimeData)

  override lazy val unparser: Unparser = new ConvertTextNumberUnparser[S](helper, numFormatFactory, e.elementRuntimeData)
}

case class ConvertTextIntegerPrim(e: ElementBase) extends ConvertTextNumberPrim[JBigInt](e) {
  val helper = new ConvertTextIntegerParserUnparserHelper[JBigInt](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextDecimalPrim(e: ElementBase) extends ConvertTextNumberPrim[JBigDecimal](e) {
  val helper = new ConvertTextDecimalParserUnparserHelper[JBigDecimal](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextNonNegativeIntegerPrim(e: ElementBase) extends ConvertTextNumberPrim[JBigInt](e) {
  val helper = new ConvertTextNonNegativeIntegerParserUnparserHelper[JBigDecimal](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextLongPrim(e: ElementBase) extends ConvertTextNumberPrim[Long](e) {
  val helper = new ConvertTextLongParserUnparserHelper[Long](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextIntPrim(e: ElementBase) extends ConvertTextNumberPrim[Int](e) {
  val helper = new ConvertTextIntParserUnparserHelper[Int](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextShortPrim(e: ElementBase) extends ConvertTextNumberPrim[Short](e) {
  val helper = new ConvertTextShortParserUnparserHelper[Short](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextBytePrim(e: ElementBase) extends ConvertTextNumberPrim[Byte](e) {
  val helper = new ConvertTextByteParserUnparserHelper[Byte](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextUnsignedLongPrim(e: ElementBase) extends ConvertTextNumberPrim[JBigInt](e) {
  val helper = new ConvertTextUnsignedLongParserUnparserHelper[JBigInt](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextUnsignedIntPrim(e: ElementBase) extends ConvertTextNumberPrim[Long](e) {
  val helper = ConvertTextUnsignedIntParserUnparserHelper[Long](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextUnsignedShortPrim(e: ElementBase) extends ConvertTextNumberPrim[Int](e) {
  val helper = new ConvertTextUnsignedShortParserUnparserHelper[Int](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextUnsignedBytePrim(e: ElementBase) extends ConvertTextNumberPrim[Short](e) {
  val helper = new ConvertTextUnsignedByteParserUnparserHelper[Short](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextDoublePrim(e: ElementBase) extends ConvertTextNumberPrim[Double](e) {
  val helper = new ConvertTextDoubleParserUnparserHelper[Double](e.textStandardZeroRep, e.ignoreCaseBool)
}

case class ConvertTextFloatPrim(e: ElementBase) extends ConvertTextNumberPrim[Float](e) {
  val helper = new ConvertTextFloatParserUnparserHelper[Float](e.textStandardZeroRep, e.ignoreCaseBool)
}
