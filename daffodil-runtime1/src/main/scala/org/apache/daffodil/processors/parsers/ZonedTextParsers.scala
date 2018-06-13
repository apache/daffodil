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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.schema.annotation.props.gen. { TextNumberCheckPolicy, TextNumberRounding, TextNumberRoundingMode, TextZonedSignStyle }
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.DecimalUtils
import java.text.ParsePosition
import com.ibm.icu.text.DecimalFormat
import org.apache.daffodil.util.MaybeDouble
import org.apache.daffodil.util.MaybeDouble
import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.Success
import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import org.apache.daffodil.processors.TermRuntimeData

case class ConvertZonedCombinatorParser(
  rd: TermRuntimeData,
  valueParser: Parser,
  converterParser: Parser)
  extends CombinatorParser(rd) {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(valueParser, converterParser)

  def parse(start: PState): Unit = {
    valueParser.parse1(start)
    if (start.processorStatus ne Success) {
      return
    }
    converterParser.parse1(start)
  }
}

case class ConvertZonedNumberParser[S](
  helper: ConvertZonedNumberParserUnparserHelperBase[S],
  nff: ZonedFormatFactoryBase[S],
  zonedSignStyle: TextZonedSignStyle,
  override val context: ElementRuntimeData) extends TextPrimParser {
  override lazy val runtimeDependencies = Nil

  override def toString = "to(xs:" + helper.xsdType + ")"

  def parse(start: PState): Unit = {
    val node: DISimple = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") {
      PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", helper.prettyType, helper.xsdType)
      return
    }

    var checkLength = str.length
    // because of the way the zero rep regular expressions are generated, they
    // will match either all or none of 'str', never part of it. Thus,
    // findFirstIn() either matches and it's a zero rep, or it doesn't and it's
    // not a zero
    val numValue = {
      val df = nff.getNumFormat(start)
      val pos = new ParsePosition(0)
      val num = try {
        val decodedNum = DecimalUtils.zonedToNumber(str, zonedSignStyle)
        if (decodedNum(0) == '-')
          checkLength = checkLength + 1
        df.get.parse(decodedNum, pos)
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case e: Exception => {
          PE(start, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s",
            helper.prettyType, helper.xsdType, str, e)
          return
        }
      }

      // Verify that what was parsed was what was passed exactly in byte count.
      // Use pos to verify all characters consumed & check for errors!
      if (num == null || pos.getIndex != checkLength) {
        PE(start, "Convert to %s (for xs:%s): Unable to parse '%s' (using up all characters).",
          helper.prettyType, helper.xsdType, str)
        return
      }

      val numValue = {
        if (helper.isInvalidRange(num)) {
          PE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
            helper.prettyType, helper.xsdType, str, num)
          return
        }

        // convert to proper type
        val asNumber = helper.getNum(num)
        Assert.invariant(!asNumber.isInstanceOf[String])

        // The following change was made because of the issues with the float
        // adding a position of precision to the Number object.  At some point we
        // will want to revert this back to the actual type but this is a quick fix
        // for the issues we were having with the 0.003 vs 0.0030 error in test_DelimProp_05
        //
        //helper.getStringFormat(asNumber)
        //
        // Above was changed back into the actual number object.
        asNumber
      }
      numValue
    }

    Assert.invariant(!numValue.isInstanceOf[String])
    node.overwriteDataValue(numValue.asInstanceOf[JNumber])

  }
}

abstract class ConvertZonedNumberParserUnparserHelperBase[S]() extends Serializable {
  val xsdType: String
  val prettyType: String

  def getNum(s: Number): S
  def isInt: Boolean
  def isInvalidRange(n: java.lang.Number): Boolean
  def getStringFormat(n: S): String
}

abstract class ConvertZonedIntegerNumberParserUnparserHelper[S]()
  extends ConvertZonedNumberParserUnparserHelperBase[S]() {
  override def isInt = true

  override def getStringFormat(n: S): String = n.toString()

  def isInvalidRange(n: java.lang.Number): Boolean = {
    //
    // Note: Scala has no class analogous to java.lang.Number. There's no common
    // base class above its number types (as there isn't above the Java *primitive* number types.)
    //
    // We're being handed here a java 'boxed' number type, and those have common parent Number.
    //
    // println("number's actual type is: " + n.getClass.getName)
    //
    // This method only for things that fit in range of a Long. (i.e., not unbounded size Integer, and not unsignedLong
    // Nevertheless, if invalid data much too long for the real numeric type is what is found in the data
    // then a java BigInteger (or maybe even BigDecimal might get passed here.
    //
    // The only thing we can check is whether there is conversion to a long available.
    // e.g., like this: Assert.invariant(n.isInstanceOf[{ def longValue : Long}])
    // But that's eliminated by erasure, so we'll just do without.
    //
    val l = n.longValue

    // check for overflow/underflow.
    val orig = new JBigDecimal(n.toString)
    val newl = new JBigDecimal(l)
    if (orig.compareTo(newl) != 0) {
      true
    } else {
      l < min || l > max
    }
  }
  def min: Long
  def max: Long
}

abstract class ConvertZonedFloatingPointNumberParserUnparserHelper[S]()
  extends ConvertZonedNumberParserUnparserHelperBase[S]() {
  override def isInt = false
  override def getStringFormat(n: S): String = {

    //val trailingZeroes = """0*(?!<[1-9])$"""
    val trailingZeroes = """(?<=[1-9])(0*)$""".r
    val trailingZeroesBeforeExponent = """(?<=[1-9])(0*?)(?=E.*)""".r

    val nAsStr = n.toString()

    if (nAsStr.contains("E") || nAsStr.contains("e")) {
      // Exponent
      trailingZeroesBeforeExponent.replaceAllIn(nAsStr, "")
    } else {
      trailingZeroes.replaceAllIn(nAsStr, "")
    }

    nAsStr
  }

}

case class ConvertZonedIntegerParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[JBigInt]() {

  override def getNum(num: Number) = new JBigInt(num.toString)
  override val xsdType = "integer"
  override val prettyType = "Unlimited Size Integer"
  override def isInvalidRange(n: java.lang.Number): Boolean = false
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertZonedNonNegativeIntegerParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[JBigInt]() {

  override def getNum(num: Number) = new JBigInt(num.toString)
  override val xsdType = "nonNegativeInteger"
  override val prettyType = "Unlimited Size Non Negative Integer"
  override def isInvalidRange(n: java.lang.Number): Boolean = {
    val value = new JBigDecimal(n.toString())
    val isNegative = value.signum == -1
    if (isNegative) return true
    false
  }
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertZonedLongParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Long]() {

  override def getNum(num: Number) = num.longValue
  override val xsdType = "long"
  override val prettyType = "Long Integer"
  val min = Long.MinValue
  val max = Long.MaxValue
}

case class ConvertZonedIntParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Int]() {

  override def getNum(num: Number) = num.intValue
  override val xsdType = "int"
  override val prettyType = "Integer"
  val min = Int.MinValue.toLong
  val max = Int.MaxValue.toLong
}

case class ConvertZonedShortParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Short]() {

  override def getNum(num: Number) = num.shortValue
  override val xsdType = "short"
  override val prettyType = "Short Integer"
  val min = Short.MinValue.toLong
  val max = Short.MaxValue.toLong
}

case class ConvertZonedByteParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Byte]() {

  override def getNum(num: Number) = num.byteValue
  override val xsdType = "byte"
  override val prettyType = "Byte"
  val min = Byte.MinValue.toLong
  val max = Byte.MaxValue.toLong
}

case class ConvertZonedUnsignedLongParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[JBigInt]() {

  override def getNum(num: Number) = new JBigInt(num.toString)
  override val xsdType = "unsignedLong"
  override val prettyType = "Unsigned Long"
  override def isInvalidRange(jn: java.lang.Number) = {
    jn match {
      case n: JBigInt => {
        n.compareTo(JBigInt.ZERO) < 0 || n.compareTo(JBigInt.ONE.shiftLeft(64)) >= 0
      }
      case _ => {
        val n = jn.longValue()
        n < 0 // note: the other side of the check is inherently ok since a Long must be smaller than an unsignedLong.
      }
    }
  }
  val min = 0.toLong
  val max = -1.toLong // unused.
}

case class ConvertZonedUnsignedIntParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Long]() {

  override def getNum(num: Number) = num.longValue
  override val xsdType = "unsignedInt"
  override val prettyType = "Unsigned Integer"
  val min = 0L
  val max = (1L << 32) - 1L
}

case class ConvertZonedUnsignedShortParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Int]() {

  override def getNum(num: Number) = num.intValue
  override val xsdType = "unsignedShort"
  override val prettyType = "Unsigned Short"
  val min = 0L
  val max = (1L << 16) - 1L
}

case class ConvertZonedUnsignedByteParserUnparserHelper[S]()
  extends ConvertZonedIntegerNumberParserUnparserHelper[Short]() {

  override def getNum(num: Number) = num.shortValue
  override val xsdType = "unsignedByte"
  override val prettyType = "Unsigned Byte"
  val min = 0L
  val max = (1L << 8) - 1L
}

case class ConvertZonedDecimalParserUnparserHelper[S]()
  extends ConvertZonedFloatingPointNumberParserUnparserHelper[JBigDecimal]() {

  override def getNum(num: Number) = new JBigDecimal(num.toString)
  override val xsdType = "decimal"
  override val prettyType = "Unlimited Size Decimal"
  override def isInvalidRange(n: java.lang.Number): Boolean = false

  override def getStringFormat(n: JBigDecimal): String = {
    n.toPlainString()
  }
}

case class ConvertZonedDoubleParserUnparserHelper[S]()
  extends ConvertZonedFloatingPointNumberParserUnparserHelper[Double]() {

  val MAX_VALUE = new JBigDecimal(Double.MaxValue)
  val MIN_VALUE = new JBigDecimal(Double.MinValue)

  override def getNum(num: Number) = num.doubleValue
  override val xsdType = "double"
  override val prettyType = "Double"
  def isInvalidRange(n: java.lang.Number): Boolean = {
    val d = n.doubleValue() // This can truncate the number and void range checking
    val bd = new JBigDecimal(n.toString)
    (d.isNaN || bd.compareTo(MIN_VALUE) < 0 || bd.compareTo(MAX_VALUE) > 0)
  }
}

case class ConvertZonedFloatParserUnparserHelper[S]()
  extends ConvertZonedFloatingPointNumberParserUnparserHelper[Float]() {

  val MAX_VALUE = new JBigDecimal(Float.MaxValue)
  val MIN_VALUE = new JBigDecimal(Float.MinValue)

  override def getNum(num: Number) = num.floatValue
  override val xsdType = "float"
  override val prettyType = "Float"
  def isInvalidRange(n: java.lang.Number): Boolean = {
    val f = n.floatValue() // This can truncated the number and void range checking
    val bd = new JBigDecimal(n.toString)
    (f.isNaN || bd.compareTo(MIN_VALUE) < 0 || bd.compareTo(MAX_VALUE) > 0)
  }
}

abstract class ZonedFormatFactoryBase[S](parserHelper: ConvertZonedNumberParserUnparserHelperBase[S]) extends Serializable {

  protected def checkUnique(
    context: ThrowsSDE) = {

    import scala.collection.mutable.{ HashMap, MultiMap, Set }

    val mm = new HashMap[String, Set[String]] with MultiMap[String, String]

    val dupes = mm.filter { case (k, s) => s.size > 1 }
    val dupeStrings = dupes.map {
      case (k, s) =>
        "Non-distinct property '%s' found in: %s".format(k, s.mkString(", "))
    }
    context.schemaDefinitionUnless(dupeStrings.size == 0, dupeStrings.mkString("\n"))
  }

  protected def generateNumFormat(
    checkPolicy: TextNumberCheckPolicy,
    pattern: String,
    rounding: TextNumberRounding,
    roundingMode: Maybe[TextNumberRoundingMode],
    roundingIncrement: MaybeDouble) = {

    val df = new DecimalFormat(pattern.replace("+", ""))

    val cp = checkPolicy match {
      case TextNumberCheckPolicy.Strict => true
      case TextNumberCheckPolicy.Lax => false
    }
    df.setParseStrict(cp)

    rounding match {
      case TextNumberRounding.Pattern => {
        df.setRoundingMode(JBigDecimal.ROUND_HALF_EVEN)
      }
      case TextNumberRounding.Explicit => {
        val rm = roundingMode.get match {
          case TextNumberRoundingMode.RoundCeiling => JBigDecimal.ROUND_CEILING
          case TextNumberRoundingMode.RoundFloor => JBigDecimal.ROUND_FLOOR
          case TextNumberRoundingMode.RoundDown => JBigDecimal.ROUND_DOWN
          case TextNumberRoundingMode.RoundUp => JBigDecimal.ROUND_UP
          case TextNumberRoundingMode.RoundHalfEven => JBigDecimal.ROUND_HALF_EVEN
          case TextNumberRoundingMode.RoundHalfDown => JBigDecimal.ROUND_HALF_DOWN
          case TextNumberRoundingMode.RoundHalfUp => JBigDecimal.ROUND_HALF_UP
          case TextNumberRoundingMode.RoundUnnecessary => JBigDecimal.ROUND_UNNECESSARY
        }
        df.setRoundingMode(rm)
        df.setRoundingIncrement(roundingIncrement.get)
      }
    }

    if (parserHelper.isInt) {
      df.setMaximumFractionDigits(0)
      df.setDecimalSeparatorAlwaysShown(false)
      df.setParseIntegerOnly(true)
    }

    df
  }

  protected def getRoundingIncrement(roundingInc: Double, context: ThrowsSDE): Double = {
    context.schemaDefinitionUnless(roundingInc >= 0, "textNumberRoundingIncrement cannot be negative")
    roundingInc
  }

  // as per ICU4J documentation, "DecimalFormat objects are not
  // synchronized. Multiple threads should not access one formatter
  // concurrently."
  def getNumFormat(state: ParseOrUnparseState): ThreadLocal[DecimalFormat]

}

class ZonedFormatFactoryStatic[S](context: ThrowsSDE,
  parserHelper: ConvertZonedNumberParserUnparserHelperBase[S],
  checkPolicy: TextNumberCheckPolicy,
  pattern: String,
  rounding: TextNumberRounding,
  roundingMode: Maybe[TextNumberRoundingMode],
  roundingIncrement: MaybeDouble)
  extends ZonedFormatFactoryBase[S](parserHelper) {

  val roundingInc: MaybeDouble = if (roundingIncrement.isEmpty) MaybeDouble.Nope else MaybeDouble { getRoundingIncrement(roundingIncrement.value, context) }

  checkUnique(
    context)

  @transient lazy val numFormat = new ThreadLocal[DecimalFormat] {
    override def initialValue() = {
      generateNumFormat(
        checkPolicy,
        pattern,
        rounding,
        roundingMode,
        roundingInc)
    }
  }

  def getNumFormat(state: ParseOrUnparseState): ThreadLocal[DecimalFormat] = {
    numFormat
  }
}
