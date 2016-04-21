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

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberCheckPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberRounding
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberRoundingMode
import edu.illinois.ncsa.daffodil.xml.XMLUtils
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
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util.MaybeDouble
import edu.illinois.ncsa.daffodil.util.MaybeDouble
import java.lang.{ Number => JNumber }

case class ConvertTextCombinatorParser(
  rd: RuntimeData,
  valueParser: Parser,
  converterParser: Parser)
  extends ParserObject(rd) {

  override lazy val childProcessors = Seq(valueParser, converterParser)

  def parse(start: PState): Unit = {
    valueParser.parse1(start)
    if (start.status ne Success) {
      return
    }
    converterParser.parse1(start)
  }
}

case class ConvertTextNumberParser[S](
  helper: ConvertTextNumberParserUnparserHelperBase[S],
  nff: NumberFormatFactoryBase[S],
  e: ElementRuntimeData) extends PrimParserObject(e) {
  override def toString = "to(xs:" + helper.xsdType + ")"

  def parse(start: PState): Unit = withParseErrorThrowing(start) {
    val node: DISimple = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") {
      PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", helper.prettyType, helper.xsdType)
      return
    }

    // because of the way the zero rep regular expressions are generated, they
    // will match either all or none of 'str', never part of it. Thus,
    // findFirstIn() either matches and it's a zero rep, or it doesn't and it's
    // not a zero
    val numValue = helper.zeroRepList.find { _.findFirstIn(str).isDefined } match {
      case Some(_) => helper.getNum(0)
      case None => {
        val df = nff.getNumFormat(start)
        val pos = new ParsePosition(0)
        val num = try {
          df.get.parse(str, pos)
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
        if (num == null || pos.getIndex != str.length) {
          PE(start, "Convert to %s (for xs:%s): Unable to parse '%s' (using up all characters).",
            helper.prettyType, helper.xsdType, str)
          return
        }

        val numValue = num match {
          // if num is infRep, -infRep, or nanRep, then parse() returns
          // Double.{POSINF, NEGINF, NAN}. otherwise, it returns some kind
          // of boxed number that can hold the full contents of the number,
          // which is one of Long, BigInteger, BigDecimal
          case d: java.lang.Double if (d.isInfinite && d > 0) => XMLUtils.PositiveInfinity
          case d: java.lang.Double if (d.isInfinite && d < 0) => XMLUtils.NegativeInfinity
          case d: java.lang.Double if (d.isNaN) => XMLUtils.NaN
          case _ => {
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
        }
        numValue
      }
    }

    Assert.invariant(!numValue.isInstanceOf[String])
    node.overwriteDataValue(numValue.asInstanceOf[JNumber])

  }
}

abstract class ConvertTextNumberParserUnparserHelperBase[S](zeroRep: List[String]) extends Serializable {
  val xsdType: String
  val prettyType: String

  def getNum(s: Number): S
  def isInt: Boolean
  def isInvalidRange(n: java.lang.Number): Boolean
  def getStringFormat(n: S): String
  def allowInfNaN: Boolean = false

  val zeroRepListRaw = zeroRep.filter { _ != "" }
  val zeroRepList = zeroRepListRaw.map { zr =>
    val d = new Delimiter()
    d.compileDelimiter(zr)
    // add '^' and '$' to require the regular expression to match the entire
    // string as a zero rep instead of just part of it
    val regex = ("^" + d.delimRegExParseDelim + "$").r
    regex
  }
}

abstract class ConvertTextIntegerNumberParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextNumberParserUnparserHelperBase[S](zeroRep) {
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
    val orig = new java.math.BigDecimal(n.toString)
    val newl = new java.math.BigDecimal(l)
    if (orig.compareTo(newl) != 0) {
      true
    } else {
      l < min || l > max
    }
  }
  def min: Long
  def max: Long
}

abstract class ConvertTextFloatingPointNumberParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextNumberParserUnparserHelperBase[S](zeroRep) {
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

case class ConvertTextIntegerParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[BigInteger](zeroRep) {

  override def getNum(num: Number) = new BigInteger(num.toString)
  override val xsdType = "integer"
  override val prettyType = "Unlimited Size Integer"
  override def isInvalidRange(n: java.lang.Number): Boolean = false
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertTextNonNegativeIntegerParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[BigInteger](zeroRep) {

  override def getNum(num: Number) = new BigInteger(num.toString)
  override val xsdType = "nonNegativeInteger"
  override val prettyType = "Unlimited Size Non Negative Integer"
  override def isInvalidRange(n: java.lang.Number): Boolean = {
    val value = BigDecimal(n.toString)
    val isNegative = value.signum == -1
    if (isNegative) return true
    false
  }
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertTextLongParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Long](zeroRep) {

  override def getNum(num: Number) = num.longValue
  override val xsdType = "long"
  override val prettyType = "Long Integer"
  val min = Long.MinValue
  val max = Long.MaxValue
}

case class ConvertTextIntParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Int](zeroRep) {

  override def getNum(num: Number) = num.intValue
  override val xsdType = "int"
  override val prettyType = "Integer"
  val min = Int.MinValue.toLong
  val max = Int.MaxValue.toLong
}

case class ConvertTextShortParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Short](zeroRep) {

  override def getNum(num: Number) = num.shortValue
  override val xsdType = "short"
  override val prettyType = "Short Integer"
  val min = Short.MinValue.toLong
  val max = Short.MaxValue.toLong
}

case class ConvertTextByteParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Byte](zeroRep) {

  override def getNum(num: Number) = num.byteValue
  override val xsdType = "byte"
  override val prettyType = "Byte"
  val min = Byte.MinValue.toLong
  val max = Byte.MaxValue.toLong
}

case class ConvertTextUnsignedLongParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[BigInteger](zeroRep) {

  override def getNum(num: Number) = new BigInteger(num.toString)
  override val xsdType = "unsignedLong"
  override val prettyType = "Unsigned Long"
  override def isInvalidRange(jn: java.lang.Number) = {
    jn match {
      case n: BigInteger => {
        n.compareTo(BigInteger.ZERO) < 0 || n.compareTo(BigInteger.ONE.shiftLeft(64)) >= 0
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

case class ConvertTextUnsignedIntParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Long](zeroRep) {

  override def getNum(num: Number) = num.longValue
  override val xsdType = "unsignedInt"
  override val prettyType = "Unsigned Integer"
  val min = 0L
  val max = (1L << 32) - 1L
}

case class ConvertTextUnsignedShortParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Int](zeroRep) {

  override def getNum(num: Number) = num.intValue
  override val xsdType = "unsignedShort"
  override val prettyType = "Unsigned Short"
  val min = 0L
  val max = (1L << 16) - 1L
}

case class ConvertTextUnsignedByteParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Short](zeroRep) {

  override def getNum(num: Number) = num.shortValue
  override val xsdType = "unsignedByte"
  override val prettyType = "Unsigned Byte"
  val min = 0L
  val max = (1L << 8) - 1L
}

case class ConvertTextDecimalParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextFloatingPointNumberParserUnparserHelper[BigDecimal](zeroRep) {

  override def getNum(num: Number) = new java.math.BigDecimal(num.toString)
  override val xsdType = "decimal"
  override val prettyType = "Unlimited Size Decimal"
  override def isInvalidRange(n: java.lang.Number): Boolean = false

  override def getStringFormat(n: BigDecimal): String = {
    n.underlying.toPlainString
  }
}

case class ConvertTextDoubleParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextFloatingPointNumberParserUnparserHelper[Double](zeroRep) {

  override def getNum(num: Number) = num.doubleValue
  override val xsdType = "double"
  override val prettyType = "Double"
  override def allowInfNaN = true
  def isInvalidRange(n: java.lang.Number): Boolean = {
    val d = n.doubleValue()
    (d.isNaN || d < Double.MinValue || d > Double.MaxValue)
  }
}

case class ConvertTextFloatParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextFloatingPointNumberParserUnparserHelper[Float](zeroRep) {

  override def getNum(num: Number) = num.floatValue
  override val xsdType = "float"
  override val prettyType = "Float"
  override def allowInfNaN = true
  def isInvalidRange(n: java.lang.Number): Boolean = {
    val f = n.floatValue()
    (f.isNaN || f < Float.MinValue || f > Float.MaxValue)
  }
}

abstract class NumberFormatFactoryBase[S](parserHelper: ConvertTextNumberParserUnparserHelperBase[S]) extends Serializable {

  protected def checkUnique(decimalSepList: Maybe[List[Character]],
    groupingSep: Maybe[Character],
    exponentRep: Maybe[String],
    infRep: Maybe[String],
    nanRep: Maybe[String],
    zeroRep: List[String],
    context: ThrowsSDE) = {

    import scala.collection.mutable.{ HashMap, MultiMap, Set }

    val mm = new HashMap[String, Set[String]] with MultiMap[String, String]
    if (decimalSepList.isDefined) {
      val dsl = decimalSepList.value
      dsl.foreach { ds => mm.addBinding(ds.toString, "textStandardDecimalSeparator") }
    }
    if (groupingSep.isDefined) mm.addBinding(groupingSep.get.toString, "textStandardGroupingSeparator")
    if (exponentRep.isDefined) {
      val er = exponentRep.value
      er.foreach { c => mm.addBinding(c.toString, "textStandardExponentRep") }
    }
    if (infRep.isDefined) {
      val ir = infRep.value
      mm.addBinding(ir, "textStandardInfinityRep")
    }
    if (nanRep.isDefined) {
      val nr = nanRep.value
      mm.addBinding(nr, "textStandardNaNRep")
    }
    zeroRep.foreach { zr => mm.addBinding(zr, "textStandardZeroRep") }

    val dupes = mm.filter { case (k, s) => s.size > 1 }
    val dupeStrings = dupes.map {
      case (k, s) =>
        "Non-distinct property '%s' found in: %s".format(k, s.mkString(", "))
    }
    context.schemaDefinitionUnless(dupeStrings.size == 0, dupeStrings.mkString("\n"))
  }

  protected def generateNumFormat(decimalSepList: Maybe[List[Character]],
    groupingSep: Maybe[Character],
    exponentRep: String,
    infRep: Maybe[String],
    nanRep: Maybe[String],
    checkPolicy: TextNumberCheckPolicy,
    pattern: String,
    rounding: TextNumberRounding,
    roundingMode: Maybe[TextNumberRoundingMode],
    roundingIncrement: MaybeDouble) = {

    val dfs = new DecimalFormatSymbols()

    if (decimalSepList.isDefined) {
      // TODO: ICU only supports a single decimal separator
      dfs.setDecimalSeparator((decimalSepList.get)(0))
    }

    if (groupingSep.isDefined) {
      dfs.setGroupingSeparator(groupingSep.get)
    }

    // TODO: this is allowed to be case insenstive, ICU doesn't support that
    dfs.setExponentSeparator(exponentRep)

    if (infRep.isDefined) {
      // TODO: this is allowed to be case insensitive, ICU doesn't support that
      dfs.setInfinity(infRep.get)
    }

    if (nanRep.isDefined) {
      // TODO: this is allowed to be case insensitive, ICU doesn't support that
      dfs.setNaN(nanRep.get)
    }

    val df = new DecimalFormat(pattern, dfs)

    val cp = checkPolicy match {
      case TextNumberCheckPolicy.Strict => true
      case TextNumberCheckPolicy.Lax => false
    }
    df.setParseStrict(cp)

    rounding match {
      case TextNumberRounding.Pattern => {
        df.setRoundingMode(java.math.BigDecimal.ROUND_HALF_EVEN)
      }
      case TextNumberRounding.Explicit => {
        val rm = roundingMode.get match {
          case TextNumberRoundingMode.RoundCeiling => java.math.BigDecimal.ROUND_CEILING
          case TextNumberRoundingMode.RoundFloor => java.math.BigDecimal.ROUND_FLOOR
          case TextNumberRoundingMode.RoundDown => java.math.BigDecimal.ROUND_DOWN
          case TextNumberRoundingMode.RoundUp => java.math.BigDecimal.ROUND_UP
          case TextNumberRoundingMode.RoundHalfEven => java.math.BigDecimal.ROUND_HALF_EVEN
          case TextNumberRoundingMode.RoundHalfDown => java.math.BigDecimal.ROUND_HALF_DOWN
          case TextNumberRoundingMode.RoundHalfUp => java.math.BigDecimal.ROUND_HALF_UP
          case TextNumberRoundingMode.RoundUnnecessary => java.math.BigDecimal.ROUND_UNNECESSARY
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

  protected def getDecimalSepList(decimalSepRaw: String, context: ThrowsSDE): List[Character] = {

    val dsl = TextStandardDecimalSeparatorCooker.cookCharacters(decimalSepRaw, context, forUnparse = false)
    // TODO: ICU only supports a single separator
    Assert.notYetImplemented(dsl.length != 1, "lists of textStandardDecimalSeparator")
    dsl
  }

  /**
   * Returns java.lang.Character on purpose since that is an AnyRef and this gets used
   * polymorphically in the CacheDynamic/Maybe frameworks which require AnyRef
   */
  protected def getGroupingSep(groupingSepRaw: String, context: ThrowsSDE): Character = {

    val gs = TextStandardGroupingSeparatorCooker.convertConstant(groupingSepRaw, context, forUnparse = false)
    gs(0)
  }

  protected def getExponentRep(exponentRepRaw: String, context: ThrowsSDE): String = {

    val er = TextStandardExponentRepCooker.convertConstant(exponentRepRaw, context, forUnparse = false)
    er
  }

  protected def getRoundingIncrement(roundingInc: Double, context: ThrowsSDE): Double = {
    context.schemaDefinitionUnless(roundingInc >= 0, "textNumberRoundingIncrement cannot be negative")
    roundingInc
  }

  // as per ICU4J documentation, "DecimalFormat objects are not
  // synchronized. Multiple threads should not access one formatter
  // concurrently."
  def getNumFormat(state: ParseOrUnparseState): ThreadLocal[NumberFormat]

}

//
// TODO: Complexity - why do we need both Static and Dynamic variants of this?
// CachedDynamic hides this distinction (or should), as does CompiledExpression underneath that.

class NumberFormatFactoryStatic[S](context: ThrowsSDE,
  parserHelper: ConvertTextNumberParserUnparserHelperBase[S],
  decimalSepExp: Maybe[CompiledExpression[String]],
  groupingSepExp: Maybe[CompiledExpression[String]],
  exponentRepExp: CompiledExpression[String],
  infRep: Maybe[String],
  nanRep: Maybe[String],
  checkPolicy: TextNumberCheckPolicy,
  pattern: String,
  rounding: TextNumberRounding,
  roundingMode: Maybe[TextNumberRoundingMode],
  roundingIncrement: MaybeDouble)
  extends NumberFormatFactoryBase[S](parserHelper) {
  Assert.invariant((!decimalSepExp.isDefined || decimalSepExp.get.isConstant) &&
    (!groupingSepExp.isDefined || groupingSepExp.get.isConstant) &&
    exponentRepExp.isConstant)

  val decSep =
    if (decimalSepExp.isEmpty) Nope else One {
      val dse = decimalSepExp.value
      getDecimalSepList(dse.constant, context)
    }

  val groupSep =
    if (groupingSepExp.isEmpty) Nope else One {
      val gse = groupingSepExp.value
      getGroupingSep(gse.constant, context)
    }

  val expRep = getExponentRep(exponentRepExp.constant, context)

  val roundingInc: MaybeDouble = if (roundingIncrement.isEmpty) MaybeDouble.Nope else MaybeDouble { getRoundingIncrement(roundingIncrement.value, context) }

  checkUnique(
    decSep,
    groupSep,
    One(expRep),
    infRep,
    nanRep,
    parserHelper.zeroRepListRaw,
    context)

  @transient lazy val numFormat = new ThreadLocal[NumberFormat] {
    override def initialValue() = {
      generateNumFormat(
        decSep,
        groupSep,
        expRep,
        infRep,
        nanRep,
        checkPolicy,
        pattern,
        rounding,
        roundingMode,
        roundingInc)
    }
  }

  def getNumFormat(state: ParseOrUnparseState): ThreadLocal[NumberFormat] = {
    numFormat
  }
}

class NumberFormatFactoryDynamic[S](staticContext: ThrowsSDE,
  parserHelper: ConvertTextNumberParserUnparserHelperBase[S],
  decimalSepExp: Maybe[CompiledExpression[String]],
  groupingSepExp: Maybe[CompiledExpression[String]],
  exponentRepExp: CompiledExpression[String],
  infRep: Maybe[String],
  nanRep: Maybe[String],
  checkPolicy: TextNumberCheckPolicy,
  pattern: String,
  rounding: TextNumberRounding,
  roundingMode: Maybe[TextNumberRoundingMode],
  roundingIncrement: MaybeDouble)
  extends NumberFormatFactoryBase[S](parserHelper)
  with Dynamic {

  val decimalSepListCached: Maybe[CachedDynamic[String, List[Character]]] =
    cacheConstantExpressionMaybe(decimalSepExp) {
      (a: String) => getDecimalSepList(a, staticContext)
    }

  val groupingSepCached: Maybe[CachedDynamic[String, Character]] =
    cacheConstantExpressionMaybe(groupingSepExp) {
      (a: String) => getGroupingSep(a, staticContext)
    }

  val exponentRepCached: CachedDynamic[String, String] =
    cacheConstantExpression(exponentRepExp) {
      (a: String) => getExponentRep(a, staticContext)
    }

  checkUnique(getStaticMaybe(decimalSepListCached),
    getStaticMaybe(groupingSepCached),
    getStatic(exponentRepCached),
    infRep,
    nanRep,
    parserHelper.zeroRepListRaw,
    staticContext)

  val roundingInc = if (roundingIncrement.isEmpty) MaybeDouble.Nope else MaybeDouble { getRoundingIncrement(roundingIncrement.value, staticContext) }

  def getNumFormat(state: ParseOrUnparseState): ThreadLocal[NumberFormat] = {

    val decimalSepList = evalWithConversionMaybe(state, decimalSepListCached) {
      (s: ParseOrUnparseState, c: String) =>
        {
          getDecimalSepList(c, s)
        }
    }

    val groupingSep = evalWithConversionMaybe(state, groupingSepCached) {
      (s: ParseOrUnparseState, c: String) =>
        {
          getGroupingSep(c, s)
        }
    }

    val exponentRep = evalWithConversion(state, exponentRepCached) {
      (s: ParseOrUnparseState, c: String) =>
        {
          getExponentRep(c, s)
        }
    }

    checkUnique(
      decimalSepList,
      groupingSep,
      One(exponentRep),
      infRep,
      nanRep,
      parserHelper.zeroRepListRaw,
      state)

    val generatedNumFormat =
      generateNumFormat(
        decimalSepList,
        groupingSep,
        exponentRep,
        infRep,
        nanRep,
        checkPolicy,
        pattern,
        rounding,
        roundingMode,
        roundingInc)

    val numFormat = new ThreadLocal[NumberFormat] {
      override def initialValue() = {
        generatedNumFormat
      }
    }

    numFormat
  }

}
