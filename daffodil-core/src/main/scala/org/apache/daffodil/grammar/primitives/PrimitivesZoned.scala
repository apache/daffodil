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
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import org.apache.daffodil.processors.parsers.ConvertZonedByteParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedCombinatorParser
import org.apache.daffodil.processors.parsers.ConvertZonedDecimalParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedIntParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedIntegerParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedLongParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedNonNegativeIntegerParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedNumberParser
import org.apache.daffodil.processors.parsers.ConvertZonedNumberParserUnparserHelperBase
import org.apache.daffodil.processors.parsers.ConvertZonedShortParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedUnsignedByteParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedUnsignedLongParserUnparserHelper
import org.apache.daffodil.processors.parsers.ConvertZonedUnsignedShortParserUnparserHelper
import org.apache.daffodil.processors.parsers.ZonedFormatFactoryBase
import org.apache.daffodil.processors.parsers.ZonedFormatFactoryStatic
import org.apache.daffodil.processors.parsers.ConvertZonedUnsignedIntParserUnparserHelper
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.schema.annotation.props.gen.TextNumberCheckPolicy
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeDouble

case class ConvertZonedCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  lazy val parser = new ConvertZonedCombinatorParser(e.termRuntimeData, value.parser, converter.parser)

  override lazy val unparser = new ConvertZonedCombinatorUnparser(e.termRuntimeData, value.unparser, converter.unparser)
}

abstract class ConvertZonedNumberPrim[S](e: ElementBase)
  extends Terminal(e, true) {

  def helper: ConvertZonedNumberParserUnparserHelperBase[S]

  def numFormatFactory: ZonedFormatFactoryBase[S] = {
    val h = helper

    val pattern = {
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
            if ((patternNoQuoted.charAt(0) != '+') && (patternNoQuoted.charAt(patternNoQuoted.length - 1) != '+'))
              e.SDE("textNumberPattern must have '+' at the beginning or the end of the pattern when textZonedFormat='zoned' and textNumberPolicy='lax' for unsigned numbers")
          }
        }
        case _ => {
          if ((patternNoQuoted.charAt(0) != '+') && (patternNoQuoted.charAt(patternNoQuoted.length - 1) != '+'))
            e.SDE("textNumberPattern must have '+' at the beginning or the end of the pattern when textZonedFormat='zoned' for signed numbers")
        }
      }

      // Load the pattern to make sure it is valid
      try {
        new DecimalFormat(p)
      } catch {
        case ex: IllegalArgumentException => e.SDE("Invalid textNumberPattern: " + ex.getMessage())
      }

      p
    }

    val (roundingIncrement: MaybeDouble, roundingMode) =
      e.textNumberRounding match {
        case TextNumberRounding.Explicit => (MaybeDouble(e.textNumberRoundingIncrement), One(e.textNumberRoundingMode))
        case TextNumberRounding.Pattern => (MaybeDouble.Nope, Nope)
      }

    val nff = new ZonedFormatFactoryStatic[S](
      e.termRuntimeData, h,
      e.textNumberCheckPolicy,
      pattern,
      e.textNumberRounding,
      roundingMode,
      roundingIncrement)
    nff
  }

  lazy val parser: Parser = new ConvertZonedNumberParser[S](helper, numFormatFactory, e.textZonedSignStyle, e.elementRuntimeData)

  override lazy val unparser: Unparser = new ConvertZonedNumberUnparser[S](helper, e.textNumberPattern, e.textZonedSignStyle, e.elementRuntimeData)
}

case class ConvertZonedIntegerPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigInt](e) {
  val helper = new ConvertZonedIntegerParserUnparserHelper[JBigInt]()
}

case class ConvertZonedDecimalPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigDecimal](e) {
  val helper = new ConvertZonedDecimalParserUnparserHelper[JBigDecimal]()
}

case class ConvertZonedNonNegativeIntegerPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigInt](e) {
  val helper = new ConvertZonedNonNegativeIntegerParserUnparserHelper[JBigDecimal]()
}

case class ConvertZonedLongPrim(e: ElementBase) extends ConvertZonedNumberPrim[Long](e) {
  val helper = new ConvertZonedLongParserUnparserHelper[Long]()
}

case class ConvertZonedIntPrim(e: ElementBase) extends ConvertZonedNumberPrim[Int](e) {
  val helper = new ConvertZonedIntParserUnparserHelper[Int]()
}

case class ConvertZonedShortPrim(e: ElementBase) extends ConvertZonedNumberPrim[Short](e) {
  val helper = new ConvertZonedShortParserUnparserHelper[Short]()
}

case class ConvertZonedBytePrim(e: ElementBase) extends ConvertZonedNumberPrim[Byte](e) {
  val helper = new ConvertZonedByteParserUnparserHelper[Byte]()
}

case class ConvertZonedUnsignedLongPrim(e: ElementBase) extends ConvertZonedNumberPrim[JBigInt](e) {
  val helper = new ConvertZonedUnsignedLongParserUnparserHelper[JBigInt]()
}

case class ConvertZonedUnsignedIntPrim(e: ElementBase) extends ConvertZonedNumberPrim[Long](e) {
  val helper = ConvertZonedUnsignedIntParserUnparserHelper[Long]()
}

case class ConvertZonedUnsignedShortPrim(e: ElementBase) extends ConvertZonedNumberPrim[Int](e) {
  val helper = new ConvertZonedUnsignedShortParserUnparserHelper[Int]()
}

case class ConvertZonedUnsignedBytePrim(e: ElementBase) extends ConvertZonedNumberPrim[Short](e) {
  val helper = new ConvertZonedUnsignedByteParserUnparserHelper[Short]()
}
