package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import java.math.BigInteger
import java.text.ParsePosition
import com.ibm.icu.text.NumberFormat
import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols



case class ConvertTextNumberParser[S](helper: ConvertTextNumberParserUnparserHelperBase[S], nff: NumberFormatFactoryBase[S], gram: Gram, e: SchemaComponent) extends PrimParser(gram, e)
{
  override def toString = "to(xs:" + helper.GramName + ")"

  def parse(start: PState): PState = withParseErrorThrowing(start) {
    val node = start.parentElement
    var str = node.dataValue

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") return PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", helper.GramDescription, helper.GramName)

    val (resultState, numAsString) = helper.zeroRepList.find { _ == str } match {
      case Some(s) => (start, helper.getStringFormat(helper.getNum(0)))
      case None => {
        val (newstate, df) = nff.getNumFormat(start)
        val pos = new ParsePosition(0)
        val num = try {
          df.parse(str, pos)
        } catch {
          case u: UnsuppressableException => throw u
          case e: Exception =>
            return PE(newstate, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s",
              helper.GramDescription, helper.GramName, str, e)
        }
            
        // Verify that what was parsed was what was passed exactly in byte count.
        // Use pos to verify all characters consumed & check for errors!
        if (num == null || pos.getIndex != str.length) {
          return PE(newstate, "Convert to %s (for xs:%s): Unable to parse '%s' (using up all characters).",
            helper.GramDescription, helper.GramName, str)
        }
    
        val asString = num match {
          // if num is infRep, -infRep, or nanRep, then parse() returns
          // Double.{POSINF, NEGINF, NAN}. otherwise, it returns some kind
          // of boxed number that can hold the full contents of the number,
          // which is one of Long, BigInteger, BigDecimal
          case d: java.lang.Double if (d.isInfinite && d > 0) => XMLUtils.PositiveInfinity
          case d: java.lang.Double if (d.isInfinite && d < 0) => XMLUtils.NegativeInfinity
          case d: java.lang.Double if (d.isNaN) => XMLUtils.NaN
          case _ => {
            if (helper.isInvalidRange(num)) {
              return PE(newstate, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
                helper.GramDescription, helper.GramName, str, num)
            }

            // convert to proper type
            val asNumber = helper.getNum(num)

            // The following change was made because of the issues with the float
            // adding a position of precision to the Number object.  At some point we
            // will want to revert this back to the actual type but this is a quick fix
            // for the issues we were having with the 0.003 vs 0.0030 error in test_DelimProp_05
            //
            //asNumber.toString
            helper.getStringFormat(asNumber)
          }
        }
        (newstate, asString)
      }
    }

    node.setDataValue(numAsString)

    resultState
  }
}

/*
 * Below is the original text number unparser. This is commented out as it has
 * no tests, and changes have been made to the parser to break compatability.
 * This will be revisited when unparsing is implemented.
 * 
 */
/*
case class ConvertTextNumberUnparser[S](helper: ConvertTextNumberParserUnparserHelperBase[S], nff: NumberFormatFactoryBase[S], e: AnnotatedSchemaComponent) extends Unparser(e)
{
  override def toString = "to(xs:" + helper.GramName + ")"

  // Converts data to number format, returns unparse exception if data cannot be converted to given format.
  def unparse(start: UState): UState = {
    // TODO: OK to get from infoset?
    var str = start.currentElement.getText //gets data from element being unparsed
    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") return UE(start, "Convert to %s (for xs:%s): Cannot unparse number from empty string", helper.GramDescription, helper.GramName)

    //make sure data can parse to appropriate type
    val df = helper.numFormat
    val pos = new ParsePosition(0)
    val num = try {
      df.parse(str, pos)
    } catch {
      case u: UnsuppressableException => throw u
      case e: Exception =>
        return UE(start, "Convert to %s (for xs:%s): Unparse of '%s' threw exception %s",
          helper.GramDescription, helper.GramName, str, e)
    }
    if (helper.isInvalidRange(num)) {
      return UE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
        helper.GramDescription, helper.GramName, str, num)
    }

    // Verify that what was unparsed was what was passed exactly in byte count.  
    // Use pos to verify all characters consumed & check for errors!
    if (pos.getIndex != str.length) {
      return UE(start, "Convert to %s (for xs:%s): Unable to unparse '%s' (using up all characters).",
        helper.GramDescription, helper.GramName, str)
    }

    // convert to proper type
    val asNumber = helper.getNum(num)

    // Verify no digits lost (the number was correctly transcribed)
    if (helper.isInt && asNumber.asInstanceOf[Number] != num) {
      // Transcription error
      return UE(start, "Convert to %s (for xs:%s): Invalid data: '%s' unparsed into %s, which converted into %s.",
        helper.GramDescription, helper.GramName, str, num, asNumber)
    }

    // TODO: Restore leading '+' sign and leading/trailing 0's, etc. (Need to overwrite number with old formatting in CharBuffer
    //      log(LogLevel.Debug, "Adding text number " + asNumber.toString))

    start
  }
}
*/

abstract class ConvertTextNumberParserUnparserHelperBase[S](zeroRep: List[String])
{
  val GramName: String
  val GramDescription: String

  def getNum(s: Number): S
  def isInt: Boolean
  def isInvalidRange(n: java.lang.Number): Boolean
  def getStringFormat(n: S): String
  def allowInfNaN: Boolean = false

  val zeroRepList = zeroRep.filter { _ != "" }
}

abstract class ConvertTextIntegerNumberParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextNumberParserUnparserHelperBase[S](zeroRep)
{
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
  extends ConvertTextNumberParserUnparserHelperBase[S](zeroRep)
{
  override def isInt = false
  override def getStringFormat(n: S): String = {

    //val trailingZeroes = """0*(?!<[1-9])$"""
    val trailingZeroes = """(?<=[1-9])(0*)$""".r
    val trailingZeroesBeforeExponent = """(?<=[1-9])(0*?)(?=E.*)""".r

    val nAsStr = n.toString()

    if (nAsStr.contains("E") || nAsStr.contains("e")) {
      // Exponent
      return trailingZeroesBeforeExponent.replaceAllIn(nAsStr, "")
    } else {
      return trailingZeroes.replaceAllIn(nAsStr, "")
    }

    nAsStr
  }

}

case class ConvertTextIntegerParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[BigInteger](zeroRep) {

  override def getNum(num: Number) = new BigInteger(num.toString)
  override val GramName = "integer"
  override val GramDescription = "Unlimited Size Integer"
  override def isInvalidRange(n: java.lang.Number): Boolean = false
  def min = -1 // ignored
  def max = -1 // ignored
}

case class ConvertTextNonNegativeIntegerParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[BigInteger](zeroRep) {

  override def getNum(num: Number) = new BigInteger(num.toString)
  override val GramName = "nonNegativeInteger"
  override val GramDescription = "Unlimited Size Non Negative Integer"
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
  override val GramName = "long"
  override val GramDescription = "Long Integer"
  val min = Long.MinValue
  val max = Long.MaxValue
}

case class ConvertTextIntParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Int](zeroRep) {

  override def getNum(num: Number) = num.intValue
  override val GramName = "int"
  override val GramDescription = "Integer"
  val min = Int.MinValue.toLong
  val max = Int.MaxValue.toLong
}

case class ConvertTextShortParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Short](zeroRep) {

  override def getNum(num: Number) = num.shortValue
  override val GramName = "short"
  override val GramDescription = "Short Integer"
  val min = Short.MinValue.toLong
  val max = Short.MaxValue.toLong
}

case class ConvertTextByteParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Byte](zeroRep) {

  override def getNum(num: Number) = num.byteValue
  override val GramName = "byte"
  override val GramDescription = "Byte"
  val min = Byte.MinValue.toLong
  val max = Byte.MaxValue.toLong
}

case class ConvertTextUnsignedLongParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[BigInteger](zeroRep) {

  override def getNum(num: Number) = new BigInteger(num.toString)
  override val GramName = "unsignedLong"
  override val GramDescription = "Unsigned Long"
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
  override val GramName = "unsignedInt"
  override val GramDescription = "Unsigned Integer"
  val min = 0L
  val max = (1L << 32) - 1L
}

case class ConvertTextUnsignedShortParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Int](zeroRep) {

  override def getNum(num: Number) = num.intValue
  override val GramName = "unsignedShort"
  override val GramDescription = "Unsigned Short"
  val min = 0L
  val max = (1L << 16) - 1L
}

case class ConvertTextUnsignedByteParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextIntegerNumberParserUnparserHelper[Short](zeroRep) {

  override def getNum(num: Number) = num.shortValue
  override val GramName = "unsignedByte"
  override val GramDescription = "Unsigned Byte"
  val min = 0L
  val max = (1L << 8) - 1L
}

case class ConvertTextDecimalParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextFloatingPointNumberParserUnparserHelper[BigDecimal](zeroRep) {

  override def getNum(num: Number) = new java.math.BigDecimal(num.toString)
  override val GramName = "decimal"
  override val GramDescription = "Unlimited Size Decimal"
  override def isInvalidRange(n: java.lang.Number): Boolean = false

  override def getStringFormat(n: BigDecimal): String = {
    n.underlying.toPlainString
  }
}

case class ConvertTextDoubleParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextFloatingPointNumberParserUnparserHelper[Double](zeroRep) {

  override def getNum(num: Number) = num.doubleValue
  override val GramName = "double"
  override val GramDescription = "Double"
  override def allowInfNaN = true
  def isInvalidRange(n: java.lang.Number): Boolean = {
    val d = n.doubleValue()
    (d.isNaN || d < Double.MinValue || d > Double.MaxValue)
  }
}

case class ConvertTextFloatParserUnparserHelper[S](zeroRep: List[String])
  extends ConvertTextFloatingPointNumberParserUnparserHelper[Float](zeroRep) {

  override def getNum(num: Number) = num.floatValue
  override val GramName = "float"
  override val GramDescription = "Float"
  override def allowInfNaN = true
  def isInvalidRange(n: java.lang.Number): Boolean = {
    val f = n.floatValue()
    (f.isNaN || f < Float.MinValue || f > Float.MaxValue)
  }
}




abstract class NumberFormatFactoryBase[S](parserHelper: ConvertTextNumberParserUnparserHelperBase[S])
{
  protected def generateNumFormat(decimalSepList: Option[List[Char]],
                                  groupingSep: Option[Char],
                                  exponentRep: String,
                                  infRep: Option[String],
                                  nanRep: Option[String],
                                  checkPolicy: TextNumberCheckPolicy,
                                  pattern: String,
                                  rounding: TextNumberRounding,
                                  roundingMode: Option[TextNumberRoundingMode],
                                  roundingIncrement: Option[Double]) = {
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
          case TextNumberRoundingMode.RoundCeiling     => java.math.BigDecimal.ROUND_CEILING
          case TextNumberRoundingMode.RoundFloor       => java.math.BigDecimal.ROUND_FLOOR
          case TextNumberRoundingMode.RoundDown        => java.math.BigDecimal.ROUND_DOWN
          case TextNumberRoundingMode.RoundUp          => java.math.BigDecimal.ROUND_UP
          case TextNumberRoundingMode.RoundHalfEven    => java.math.BigDecimal.ROUND_HALF_EVEN
          case TextNumberRoundingMode.RoundHalfDown    => java.math.BigDecimal.ROUND_HALF_DOWN
          case TextNumberRoundingMode.RoundHalfUp      => java.math.BigDecimal.ROUND_HALF_UP
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

  protected def getDecimalSepList(decimalSep: String, context: ThrowsSDE): List[Char] = {
    // TODO: raw byte eneityt not allowed
    // TODO: the below aren't correct checks. For example, %%NL; is allowed
    //       (escaped %), but would still cause an SDE
    context.schemaDefinitionUnless(decimalSep.length > 0, "textStandardDecimalSeparator must not be empty")
    context.schemaDefinitionUnless(!decimalSep.contains("%NL;"), "textStandardDecimalSeparator cannot contain NL")
    context.schemaDefinitionUnless(!decimalSep.contains("%WSP;"), "textStandardDecimalSeparator cannot contain WSP")
    context.schemaDefinitionUnless(!decimalSep.contains("%WSP*;"), "textStandardDecimalSeparator cannot contain WSP*")
    context.schemaDefinitionUnless(!decimalSep.contains("%WSP+;"), "textStandardDecimalSeparator cannot contain WSP+")
    context.schemaDefinitionUnless(!decimalSep.contains("%ES;"), "textStandardDecimalSeparator cannot contain ES")

    val dsl = new ListOfSingleCharacterLiteral(decimalSep, context).cooked
    // TODO: ICU only supports a single separator
    Assert.notYetImplemented(dsl.length != 1, "lists of textStandardDeciamalSeparator")
    dsl
  }

  protected def getGroupingSep(groupingSep: String, context: ThrowsSDE): Char = {
    // TODO: raw byte entity not allowed
    // TODO: the below aren't correct checks. For example, %%NL; is allowed
    //       (escaped %), but would still cause an SDE
    context.schemaDefinitionUnless(groupingSep.length > 0, "textStandardGroupingSeparator must not be empty")
    context.schemaDefinitionUnless(!groupingSep.contains("%NL;"), "textStandardGroupingSeparator cannot contain NL")
    context.schemaDefinitionUnless(!groupingSep.contains("%WSP;"), "textStandardGroupingSeparator cannot contain WSP")
    context.schemaDefinitionUnless(!groupingSep.contains("%WSP*;"), "textStandardGroupingSeparator cannot contain WSP*")
    context.schemaDefinitionUnless(!groupingSep.contains("%WSP+;"), "textStandardGroupingSeparator cannot contain WSP+")
    context.schemaDefinitionUnless(!groupingSep.contains("%ES;"), "textStandardGroupingSeparator cannot contain ES")

    val gs = new SingleCharacterLiteral(groupingSep, context).cooked
    gs(0)
  }

  protected def getExponentRep(exponentRep: String, context: ThrowsSDE): String = {
    // TODO: raw byte entity not allowed
    // TODO: the below aren't correct checks. For example, %%NL; is allowed
    //       (escaped %), but would still cause an SDE
    context.schemaDefinitionUnless(!exponentRep.contains("%NL;"), "textStandardExponentRep cannot contain NL")
    context.schemaDefinitionUnless(!exponentRep.contains("%WSP;"), "textStandardExponentRep cannot contain WSP")
    context.schemaDefinitionUnless(!exponentRep.contains("%WSP*;"), "textStandardExponentRep cannot contain WSP*")
    context.schemaDefinitionUnless(!exponentRep.contains("%WSP+;"), "textStandardExponentRep cannot contain WSP+")
    context.schemaDefinitionUnless(!exponentRep.contains("%ES;"), "textStandardExponentRep cannot contain ES")

    val er = new StringValueAsLiteral(exponentRep, context).cooked
    er
  }

  protected def getRoundingIncrement(roundingInc: Double, context: ThrowsSDE): Double = {
    context.schemaDefinitionUnless(roundingInc >= 0, "textNumberRoundingIncrement cannot be negative") 
    roundingInc
  }

  def getNumFormat(state: PState): (PState, NumberFormat)

}

class NumberFormatFactoryStatic[S](context: ThrowsSDE,
                                   parserHelper: ConvertTextNumberParserUnparserHelperBase[S],
                                   decimalSepExp: Option[CompiledExpression],
                                   groupingSepExp: Option[CompiledExpression],
                                   exponentRepExp: CompiledExpression,
                                   infRep: Option[String],
                                   nanRep: Option[String],
                                   checkPolicy: TextNumberCheckPolicy,
                                   pattern: String,
                                   rounding: TextNumberRounding,
                                   roundingMode: Option[TextNumberRoundingMode],
                                   roundingIncrement: Option[Double])
  extends NumberFormatFactoryBase[S](parserHelper)
{
  Assert.invariant((!decimalSepExp.isDefined || decimalSepExp.get.isConstant) &&
                   (!groupingSepExp.isDefined || groupingSepExp.get.isConstant) &&
                   exponentRepExp.isConstant)

  val decSep = decimalSepExp.map { dse => getDecimalSepList(dse.constantAsString, context) }

  val groupSep = groupingSepExp.map { gse => getGroupingSep(gse.constantAsString, context) }

  val expSep = getExponentRep(exponentRepExp.constantAsString, context)

  val roundingInc = roundingIncrement.map { ri => getRoundingIncrement(ri, context) }

  val numFormat = generateNumFormat(decSep,
                                    groupSep,
                                    expSep,
                                    infRep,
                                    nanRep,
                                    checkPolicy,
                                    pattern,
                                    rounding,
                                    roundingMode,
                                    roundingInc)

  def getNumFormat(state: PState) = {
    (state, numFormat)
  }
}


class NumberFormatFactoryDynamic[S](staticContext: ThrowsSDE,
                                    parserHelper: ConvertTextNumberParserUnparserHelperBase[S],
                                    decimalSepExp: Option[CompiledExpression],
                                    groupingSepExp: Option[CompiledExpression],
                                    exponentRepExp: CompiledExpression,
                                    infRep: Option[String],
                                    nanRep: Option[String],
                                    checkPolicy: TextNumberCheckPolicy,
                                    pattern: String,
                                    rounding: TextNumberRounding,
                                    roundingMode: Option[TextNumberRoundingMode],
                                    roundingIncrement: Option[Double])
  extends NumberFormatFactoryBase[S](parserHelper)
{
  // Returns an Either, with Right being the value of the constant, and the
  // Left being the a non-constant compiled expression. The conv variable is
  // used to convert the constant value to a more usable form, and perform and
  // SDE checks. This should be called during initialization/compile time. Not
  // during runtime.
  def cacheConstantExpression[A](e: CompiledExpression)(conv: (Any) => A): Either[CompiledExpression, A] = {
    if (e.isConstant) {
      val v: Any = e.constant
      Right(conv(v))
    } else {
      Left(e)
    }
  }

  // This is the same as cacheConstantExpression, except it works on an
  // optional value
  def cacheOptionalConstantExpression[A](oe: Option[CompiledExpression])(conv: (Any) => A): Option[Either[CompiledExpression, A]] = {
    oe.map { e => cacheConstantExpression[A](e)(conv) }
  }

  // For any expression that couldn't be evaluated in cacheConstantExpression,
  // this evaluates that. This is used to evaluate only runtime expressions.
  // This also carries along PState that is modified during expression
  // evaluation.
  def evalWithConversion[A](s: PState, e: Either[CompiledExpression, A])(conv: (PState, Any) => A): (PState, A) = {
    e match {
      case Right(r) => (s, r)
      case Left(l) => {
        val R(aAsAny, newVMap) = l.evaluate(s.parentElement, s.variableMap, s)
        val a: A = conv(s, aAsAny)
        (s.withVariables(newVMap), a)
      }
    }
  }

  // This is the same as evalWithConversion, except it works on an Optional
  // value.
  def evalOptionWithConversion[A](s: PState, oe: Option[Either[CompiledExpression, A]])(conv: (PState, Any) => A): (PState, Option[A]) = {
    oe match {
      case Some(e) => {
        val (s1, a) = evalWithConversion[A](s, e)(conv)
        (s1, Some(a))
      }
      case None => (s, None)
    }
  }


  val decimalSepListOptEither: Option[Either[CompiledExpression, List[Char]]] =
    cacheOptionalConstantExpression[List[Char]](decimalSepExp) {
      (a: Any) => getDecimalSepList(a.asInstanceOf[String], staticContext)
    }

  val groupingSepOptEither: Option[Either[CompiledExpression, Char]] =
    cacheOptionalConstantExpression[Char](groupingSepExp) {
      (a: Any) => getGroupingSep(a.asInstanceOf[String], staticContext)
    }

  val exponentRepEither: Either[CompiledExpression, String] =
    cacheConstantExpression[String](exponentRepExp) {
      (a: Any) => getExponentRep(a.asInstanceOf[String], staticContext)
    }

  val roundingInc = roundingIncrement.map { ri => getRoundingIncrement(ri, staticContext) }

  def getNumFormat(state: PState): (PState, NumberFormat) = {

    val (decimalSepState, decimalSepList) = evalOptionWithConversion[List[Char]](state, decimalSepListOptEither) {
      (s: PState, c: Any) => {
        getDecimalSepList(c.asInstanceOf[String], s)
      }
    }

    val (groupingSepState, groupingSep) = evalOptionWithConversion[Char](decimalSepState, groupingSepOptEither) {
      (s: PState, c: Any) => {
        getGroupingSep(c.asInstanceOf[String], s)
      }
    }

    val (exponentRepState, exponentRep) = evalWithConversion[String](groupingSepState, exponentRepEither) {
      (s: PState, c: Any) => {
        getExponentRep(c.asInstanceOf[String], s)
      }
    }

    val numFormat = generateNumFormat(decimalSepList,
                                      groupingSep,
                                      exponentRep,
                                      infRep,
                                      nanRep,
                                      checkPolicy,
                                      pattern,
                                      rounding,
                                      roundingMode,
                                      roundingInc)
    (exponentRepState, numFormat)
  }
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
        case ex: Exception => e.SDE("Invalid textNumberPattern: " + ex.getMessage)
      }

      (p, patternNoQuoted)
    }

    val (roundingIncrement, roundingMode) =
      e.textNumberRounding match {
        case TextNumberRounding.Explicit => (Some(e.textNumberRoundingIncrement), Some(e.textNumberRoundingMode))
        case TextNumberRounding.Pattern => (None, None)
      }

    val (infRep, nanRep) =
      if (h.allowInfNaN) {
        (Some(e.textStandardInfinityRep), Some(e.textStandardNaNRep))
      } else {
        (None, None)
      }

    val decSep =
      if (!h.isInt && (patternStripped.contains(".") ||
                       patternStripped.contains("E") ||
                       patternStripped.contains("@"))) {
        Some(e.textStandardDecimalSeparator)
      } else {
        None
      }

    val groupSep =
      if (patternStripped.contains(",")) {
        Some(e.textStandardGroupingSeparator)
      } else {
        None
      }

    val isConstant = ((decSep.isEmpty || decSep.get.isConstant) &&
                      (groupSep.isEmpty || groupSep.get.isConstant) &&
                      e.textStandardExponentRep.isConstant)

    val nff = if (isConstant) {
        new NumberFormatFactoryStatic[S](e, h,
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
       new NumberFormatFactoryDynamic[S](e, h,
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

  def parser: Parser = new ConvertTextNumberParser[S](helper, numFormatFactory, this, e)

  def unparser: Unparser = DummyUnparser(e) //new ConvertTextNumberUnparser[S](helper, numFormatFactory, e)
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
