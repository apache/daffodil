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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberCheckPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberRounding
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberRoundingMode
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.math.BigInteger
import java.text.ParsePosition
import com.ibm.icu.text.NumberFormat
import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ConvertTextNumberUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ConvertTextCombinatorUnparser
import edu.illinois.ncsa.daffodil.util.MaybeDouble

case class ConvertTextCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  def parser = new ConvertTextCombinatorParser(e.runtimeData, value.parser, converter.parser)

  override def unparser = new ConvertTextCombinatorUnparser(e.runtimeData, value.unparser, converter.unparser)
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
        val nf = new DecimalFormat(p)
      } catch {
        case ex: IllegalArgumentException => e.SDE("Invalid textNumberPattern: " + ex.getMessage)
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

    val decSep =
      if (!h.isInt && (patternStripped.contains(".") ||
        patternStripped.contains("E") ||
        patternStripped.contains("@"))) {
        One(e.textStandardDecimalSeparator)
      } else {
        Nope
      }

    val groupSep =
      if (patternStripped.contains(",")) {
        One(e.textStandardGroupingSeparator)
      } else {
        Nope
      }

    val isConstant = ((decSep.isEmpty || decSep.get.isConstant) &&
      (groupSep.isEmpty || groupSep.get.isConstant) &&
      e.textStandardExponentRep.isConstant)

    val nff = if (isConstant) {
      new NumberFormatFactoryStatic[S](e.runtimeData, h,
        decSep,
        groupSep,
        e.textStandardExponentRep,
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
        e.textStandardExponentRep,
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

  def parser: Parser = new ConvertTextNumberParser[S](helper, numFormatFactory, e.elementRuntimeData)

  override def unparser: Unparser = new ConvertTextNumberUnparser[S](helper, numFormatFactory, e.elementRuntimeData)
}

case class ConvertTextIntegerPrim(e: ElementBase) extends ConvertTextNumberPrim[BigInteger](e) {
  val helper = new ConvertTextIntegerParserUnparserHelper[BigInteger](e.textStandardZeroRep)
}

case class ConvertTextDecimalPrim(e: ElementBase) extends ConvertTextNumberPrim[BigDecimal](e) {
  val helper = new ConvertTextDecimalParserUnparserHelper[BigDecimal](e.textStandardZeroRep)
}

case class ConvertTextNonNegativeIntegerPrim(e: ElementBase) extends ConvertTextNumberPrim[BigInteger](e) {
  val helper = new ConvertTextNonNegativeIntegerParserUnparserHelper[BigDecimal](e.textStandardZeroRep)
}

case class ConvertTextLongPrim(e: ElementBase) extends ConvertTextNumberPrim[Long](e) {
  val helper = new ConvertTextLongParserUnparserHelper[Long](e.textStandardZeroRep)
}

case class ConvertTextIntPrim(e: ElementBase) extends ConvertTextNumberPrim[Int](e) {
  val helper = new ConvertTextIntParserUnparserHelper[Int](e.textStandardZeroRep)
}

case class ConvertTextShortPrim(e: ElementBase) extends ConvertTextNumberPrim[Short](e) {
  val helper = new ConvertTextShortParserUnparserHelper[Short](e.textStandardZeroRep)
}

case class ConvertTextBytePrim(e: ElementBase) extends ConvertTextNumberPrim[Byte](e) {
  val helper = new ConvertTextByteParserUnparserHelper[Byte](e.textStandardZeroRep)
}

case class ConvertTextUnsignedLongPrim(e: ElementBase) extends ConvertTextNumberPrim[BigInteger](e) {
  val helper = new ConvertTextUnsignedLongParserUnparserHelper[BigInteger](e.textStandardZeroRep)
}

case class ConvertTextUnsignedIntPrim(e: ElementBase) extends ConvertTextNumberPrim[Long](e) {
  val helper = ConvertTextUnsignedIntParserUnparserHelper[Long](e.textStandardZeroRep)
}

case class ConvertTextUnsignedShortPrim(e: ElementBase) extends ConvertTextNumberPrim[Int](e) {
  val helper = new ConvertTextUnsignedShortParserUnparserHelper[Int](e.textStandardZeroRep)
}

case class ConvertTextUnsignedBytePrim(e: ElementBase) extends ConvertTextNumberPrim[Short](e) {
  val helper = new ConvertTextUnsignedByteParserUnparserHelper[Short](e.textStandardZeroRep)
}

case class ConvertTextDoublePrim(e: ElementBase) extends ConvertTextNumberPrim[Double](e) {
  val helper = new ConvertTextDoubleParserUnparserHelper[Double](e.textStandardZeroRep)
}

case class ConvertTextFloatPrim(e: ElementBase) extends ConvertTextNumberPrim[Float](e) {
  val helper = new ConvertTextFloatParserUnparserHelper[Float](e.textStandardZeroRep)
}
