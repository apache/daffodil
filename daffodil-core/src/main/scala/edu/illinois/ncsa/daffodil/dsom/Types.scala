package edu.illinois.ncsa.daffodil.dsom

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

import java.math.BigInteger
import scala.collection.mutable.Queue
import scala.util.matching.Regex
import scala.xml._
import com.ibm.icu.text.DateFormat
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.HasIsError
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Enum
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.util.Misc

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase
  extends HasIsError

sealed trait SimpleTypeBase
  extends TypeBase
  with TypeChecks {
  def context: SchemaComponent
  def primitiveType: PrimitiveType
}

trait TypeConversions extends TypeChecks {

  private def tryCatch[T](context: ThrowsSDE, msg: String, args: Any*)(f: => T): T = {
    try { f } catch {
      case u: UnsuppressableException => throw u
      case n: Exception => context.SDE(msg, args: _*) // TODO: Internationalization
    }
  }

  def convertToLong(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Long.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Long, context)
      if (!isSuccess) context.SDE("Cannot convert %s to Long. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toLong
        case None => Assert.impossibleCase
      }
    }

  def convertToDouble(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Double.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Double, context)
      if (!isSuccess) context.SDE("Cannot convert %s to Double. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toDouble
        case None => Assert.impossibleCase
      }
    }

  def convertToBoolean(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Boolean.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Boolean, context)
      if (!isSuccess) context.SDE("Cannot convert %s to Boolean.", s.toString)
      theValue match {
        case Some(bd) => bd.toInt match {
          case 0 => false
          case 1 => true
          case _ => Assert.impossibleCase
        }
        case None => Assert.impossibleCase
      }
    }

  def convertToByte(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Byte.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Byte, context)
      if (!isSuccess) context.SDE("Cannot convert %s to Byte. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toByte
        case None => Assert.impossibleCase
      }
    }

  def convertToShort(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Short.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Short, context)
      if (!isSuccess) context.SDE("Cannot convert %s to Short. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toShort
        case None => Assert.impossibleCase
      }
    }

  def convertToInt(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Int.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Int, context)
      if (!isSuccess) context.SDE("Cannot convert %s to UnsignedInt. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toInt
        case None => Assert.impossibleCase
      }
    }

  def convertToUByte(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to UnsignedByte.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.UByte, context)
      if (!isSuccess) context.SDE("Cannot convert %s to UnsignedByte. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toShort
        case None => Assert.impossibleCase
      }
    }

  def convertToUShort(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to UnsignedShort.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.UShort, context)
      if (!isSuccess) context.SDE("Cannot convert %s to UnsignedShort. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toInt
        case None => Assert.impossibleCase
      }
    }

  def convertToUInt(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to UnsignedInt.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.UInt, context)
      if (!isSuccess) context.SDE("Cannot convert %s to UnsignedInt. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd.toLong
        case None => Assert.impossibleCase
      }
    }

  def convertToULong(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to UnsignedLong.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.ULong, context)
      if (!isSuccess) context.SDE("Cannot convert %s to UnsignedLong. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd
        case None => Assert.impossibleCase
      }
    }

  def convertToInteger(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Integer (Unbounded).", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.Integer, context)
      if (!isSuccess) context.SDE("Cannot convert %s to Integer (Unbounded). Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd
        case None => Assert.impossibleCase
      }
    }

  def convertToNonNegativeInteger(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to NonNegativeInteger.", s) {
      val (isSuccess, theValue) = this.checkRangeReturnsValue(s.toString, PrimType.NonNegativeInteger, context)
      if (!isSuccess) context.SDE("Cannot convert %s to NonNegativeInteger. Failed range checks.", s.toString)
      theValue match {
        case Some(bd) => bd
        case None => Assert.impossibleCase
      }
    }

  def convertToDecimal(s: Any, context: ThrowsSDE) =
    tryCatch(context, "Cannot convert %s to Decimal.", s) {
      val value = BigDecimal(s.toString)
      value
    }

}

trait TypeChecks {
  protected def dateToBigDecimal(date: String, format: String, dateType: String, context: ThrowsSDE): java.math.BigDecimal = {
    val df = new SimpleDateFormat(format)
    df.setCalendar(new GregorianCalendar())
    df.setTimeZone(TimeZone.GMT_ZONE)
    val bd = try {
      val dt = df.parse(date)
      new java.math.BigDecimal(dt.getTime())
    } catch {
      case e: Exception => {
        try {
          // Could already be a BigDecimal
          new java.math.BigDecimal(date)
        } catch {
          case e: Exception => context.SDE("Failed to parse (%s) to %s (%s)", date, dateType, format)
        }

      }
    }
    bd
  }

  private def convertStringToBigDecimal(value: String, primitiveType: PrimType.Type, context: ThrowsSDE): java.math.BigDecimal = {
    primitiveType match {
      case PrimType.DateTime => dateToBigDecimal(value, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx", PrimType.DateTime.toString(), context)
      case PrimType.Date => dateToBigDecimal(value, "uuuu-MM-ddxxx", PrimType.Date.toString(), context)
      case PrimType.Time => dateToBigDecimal(value, "HH:mm:ss.SSSSSSxxx", PrimType.Time.toString(), context)
      case _ => new java.math.BigDecimal(value)
    }
  }

  def checkRangeReturnsValue(value: String, primitiveType: PrimType.Type, theContext: ThrowsSDE): (Boolean, Option[BigDecimal]) = {
    // EmptyString is only valid for hexBinary and String
    if ((value == null | value.length() == 0)) {
      return primitiveType match {
        case PrimType.HexBinary | PrimType.String => (true, None)
        case _ => (false, None)
      }
    }

    // Don't need to range check String or HexBinary
    // no point attempting a conversion to BigDecimal so
    // return early here.
    primitiveType match {
      case PrimType.String | PrimType.HexBinary => return (true, None)
      case _ => /* Continue on below */
    }

    // Check Boolean, and the Numeric types.
    (value.toLowerCase(), primitiveType) match {
      case ("true", PrimType.Boolean) => (true, Some(BigDecimal(1)))
      case ("false", PrimType.Boolean) => (true, Some(BigDecimal(0)))
      case (x, PrimType.Boolean) => theContext.SDE("%s is not a valid Boolean value. Expected 'true' or 'false'.", x)
      case (_, _) => {
        // Perform conversions once
        val theValue = convertStringToBigDecimal(value, primitiveType, theContext)

        // Here we're just doing range checking for the
        // specified primitive type
        val res: Boolean = primitiveType match {
          case PrimType.Int => isInIntRange(theValue)
          case PrimType.Byte => isInByteRange(theValue)
          case PrimType.Short => isInShortRange(theValue)
          case PrimType.Long => isInLongRange(theValue)
          case PrimType.Integer => true // Unbounded Integer
          case PrimType.UInt => isInUnsignedIntRange(theValue)
          case PrimType.UByte => isInUnsignedByteRange(theValue)
          case PrimType.UShort => isInUnsignedShortRange(theValue)
          case PrimType.ULong => isInUnsignedLongRange(theValue)
          case PrimType.Double => isInDoubleRange(theValue)
          case PrimType.Float => isInFloatRange(theValue)
          case PrimType.DateTime => true
          case PrimType.Date => true
          case PrimType.Time => true
          case PrimType.Boolean => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.Decimal => true // Unbounded Decimal
          case PrimType.HexBinary => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.String => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.NonNegativeInteger => isInNonNegativeIntegerRange(theValue)
        }
        val isValueWhole = {
          val IsWholeRegex = """^[^.]*(\.0*)?$""".r
          value match {
            case IsWholeRegex(_) => true
            case _ => false
          }
        }
        primitiveType match {
          case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
            PrimType.Integer | PrimType.UInt | PrimType.UByte | PrimType.UShort |
            PrimType.ULong => if (!isValueWhole) theContext.SDE("checkRange - Value (%s) must be a whole number.", value)
          case _ => // OK
        }
        (res, Some(theValue))
      }
    }
  }

  def checkRange(value: String, primitiveType: PrimType.Type, theContext: ThrowsSDE): Boolean = {
    val (boolResult, _) = checkRangeReturnsValue(value, primitiveType, theContext)
    boolResult
  }

  protected def isNumInRange(num: java.math.BigDecimal, min: java.math.BigDecimal,
    max: java.math.BigDecimal): Boolean = {
    val checkMin = num.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    val checkMax = num.compareTo(max)
    if (checkMax > 0) { return false } // num greater than max
    true
  }
  protected def isInByteRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Byte.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Byte.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInShortRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Short.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Short.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntegerRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    // Unbounded Integer
    val checkMin = value.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    true
  }
  protected def isInLongRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Long.MinValue.toString())
    val max = new java.math.BigDecimal(Long.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInDoubleRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Double.MinValue.toString())
    val max = new java.math.BigDecimal(Double.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInFloatRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Float.MinValue.toString())
    val max = new java.math.BigDecimal(Float.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInDecimalRange(value: java.math.BigDecimal): Boolean = {
    // BigDecimal is unbounded? So nothing outside of its range?
    true
  }
  protected def isInNegativeIntegerRange(value: java.math.BigDecimal, context: ThrowsSDE): Boolean = {
    // TODO: NegativeInteger not supported in DFDL v1.0
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    val isNegative = value.signum == -1
    if (!isNegative) context.SDE("Expected a negative integer for this value.")
    val checkMin = value.compareTo(min)
    if (checkMin < 0) context.SDE("Value (%s) was found to be more negative than allowed by Int.MinValue.", value.intValue())
    true
  }
  protected def isInNonNegativeIntegerRange(value: java.math.BigDecimal): Boolean = {
    // Should be treated as unsigned Integer (unbounded)
    val min = java.math.BigDecimal.ZERO
    val isNegative = value.signum == -1
    if (isNegative) return false
    true
  }
  protected def isInUnsignedXXXRange(value: java.math.BigDecimal, numBits: Int, typeName: String): Boolean = {
    Assert.usage(numBits <= 64, "isInUnsignedXXXRange: numBits must be <= 64.")
    val min = java.math.BigDecimal.ZERO
    val max = new java.math.BigDecimal(BigInteger.ONE.shiftLeft(numBits)).subtract(new java.math.BigDecimal(1))
    val isNegative = value.signum == -1
    if (isNegative) return false
    val checkMax = value.compareTo(max)
    if (checkMax > 0) return false
    true
  }
  protected def isInUnsignedLongRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 64, "ulong")

  protected def isInUnsignedIntRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 32, "uint")

  protected def isInUnsignedShortRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 16, "ushort")

  protected def isInUnsignedByteRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 8, "ubyte")
}

trait Facets { self: SimpleTypeDefBase =>
  import Facet._
  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  def retrieveFacetValueFromRestrictionBase(xml: Node, facetName: Facet.Type): String = {
    val res = xml \\ "restriction" \ facetName.toString() \ "@value"
    if (res.length > 0) res.head.text else ""
  }
  def retrieveFacetValuesFromRestrictionBase(xml: Node, facetName: Facet.Type): Seq[String] = {
    val res = xml \\ "restriction" \ facetName.toString() \\ "@value"
    if (res.length > 0) res.map(n => n.text).toList else List.empty
  }
  def enumeration(xml: Node): Seq[String] = { retrieveFacetValuesFromRestrictionBase(xml, Facet.enumeration) }
  def fractionDigits(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.fractionDigits) }
  def maxExclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.maxExclusive) }
  def maxInclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.maxInclusive) }
  def maxLength(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.maxLength) }
  def minExclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.minExclusive) }
  def minInclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.minInclusive) }
  def minLength(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.minLength) }
  def pattern(xml: Node): Seq[String] = {
    // Patterns are OR'd locally, AND'd remotely
    retrieveFacetValuesFromRestrictionBase(xml, Facet.pattern).map(p => p)
  }
  def totalDigits(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.totalDigits) }
  def whitespace(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.whiteSpace) }

  lazy val localPatternValue: String = {
    // Patterns within a type are OR'd
    // per http://www.xfront.com/XML-Schema-library/papers/Algorithm-for-Merging-a-simpleType-Dependency-Chain.pdf
    //
    // Assumed to be valid RegEx
    val patterns = pattern(xml)
    patterns.mkString("|")
  }
  lazy val localMinInclusiveValue: String = minInclusive(xml)
  lazy val localMaxInclusiveValue: String = maxInclusive(xml)
  lazy val localMinExclusiveValue: String = minExclusive(xml)
  lazy val localMaxExclusiveValue: String = maxExclusive(xml)
  lazy val localMinLengthValue: String = minLength(xml)
  lazy val localMaxLengthValue: String = maxLength(xml)
  lazy val localTotalDigitsValue: String = totalDigits(xml)
  lazy val localFractionDigitsValue: String = fractionDigits(xml)
  lazy val localEnumerationValue: String = {
    // Enumerations are OR'd
    // May be empty string
    // Must be unique
    val enumerations = enumeration(xml)
    val distinctEnums = enumerations.distinct
    if (enumerations.size != distinctEnums.size) context.SDE("Enumerations must be unique!")
    // Not a regular expression, but we plan to use it as one
    // so we must escape characters that can be interpreted as RegEx
    enumerations.map(s => escapeForRegex(s)).mkString("|")
  }
  lazy val localWhitespaceValue: String = {
    whitespace(xml)
    context.SDE("whitespaceValue is not implemented for DFDL v1.0 schemas but reserved for future use.")
  }

  private def escapeForRegex(s: String): String = {
    val sb = new StringBuilder
    s.foreach(c => {
      c match {
        case '[' => sb.append("\\[")
        case '\\' => sb.append("\\\\")
        case '^' => sb.append("\\^")
        case '$' => sb.append("\\$")
        case '.' => sb.append("\\.")
        case '|' => sb.append("\\|")
        case '?' => sb.append("\\?")
        case '*' => sb.append("\\*")
        case '+' => sb.append("\\+")
        case '(' => sb.append("\\(")
        case ')' => sb.append("\\)")
        case '{' => sb.append("\\{")
        case '}' => sb.append("\\}")
        case x => sb.append(x)
      }
    })
    sb.toString()
  }

  lazy val hasEnumeration: Boolean = (localEnumerationValue.length > 0) || (getRemoteFacetValues(Facet.enumeration).size > 0)
  lazy val hasPattern: Boolean = (localPatternValue.length > 0) || (getRemoteFacetValues(Facet.pattern).size > 0)
  lazy val hasMinLength: Boolean = (localMinLengthValue != "") || (getRemoteFacetValues(Facet.minLength).size > 0)
  lazy val hasMaxLength: Boolean = (localMaxLengthValue != "") || (getRemoteFacetValues(Facet.maxLength).size > 0)
  lazy val hasMinInclusive: Boolean = (localMinInclusiveValue != "") || (getRemoteFacetValues(Facet.minInclusive).size > 0)
  lazy val hasMaxInclusive: Boolean = (localMaxInclusiveValue != "") || (getRemoteFacetValues(Facet.maxInclusive).size > 0)
  lazy val hasMinExclusive: Boolean = (localMinExclusiveValue != "") || (getRemoteFacetValues(Facet.minExclusive).size > 0)
  lazy val hasMaxExclusive: Boolean = (localMaxExclusiveValue != "") || (getRemoteFacetValues(Facet.maxExclusive).size > 0)
  lazy val hasTotalDigits: Boolean = (localTotalDigitsValue != "") || (getRemoteFacetValues(Facet.totalDigits).size > 0)
  lazy val hasFractionDigits: Boolean = (localFractionDigitsValue != "") || (getRemoteFacetValues(Facet.fractionDigits).size > 0)

  lazy val patternValues: Seq[FacetValueR] = {
    val values = combinedBaseFacets.filter { case (f, _) => f == Facet.pattern }
    if (values.size > 0) {
      val res: Seq[FacetValueR] = values.map { case (f, v) => (f, v.r) }
      res
    } else Seq.empty
  }
  lazy val enumerationValues: String = {
    // Should only ever have one set per SimpleType
    val values = combinedBaseFacets.filter { case (f, _) => f == Facet.enumeration }
    if (values.size > 0) {
      val (_, value) = values(0)
      value
    } else context.SDE("Enumeration was not found in this context.")
  }
  // TODO: Tidy up.  Can likely replace getFacetValue with a similar call to combinedBaseFacets
  // as combinedBaseFacets should contain the 'narrowed' values.
  //
  lazy val minLengthValue: java.math.BigDecimal = getFacetValue(localMinLengthValue, Facet.minLength, hasMinLength)
  lazy val maxLengthValue: java.math.BigDecimal = getFacetValue(localMaxLengthValue, Facet.maxLength, hasMaxLength)
  lazy val minInclusiveValue: java.math.BigDecimal = getFacetValue(localMinInclusiveValue, Facet.minInclusive, hasMinInclusive)
  lazy val maxInclusiveValue: java.math.BigDecimal = getFacetValue(localMaxInclusiveValue, Facet.maxInclusive, hasMaxInclusive)
  lazy val minExclusiveValue: java.math.BigDecimal = getFacetValue(localMinExclusiveValue, Facet.minExclusive, hasMinExclusive)
  lazy val maxExclusiveValue: java.math.BigDecimal = getFacetValue(localMaxExclusiveValue, Facet.maxExclusive, hasMaxExclusive)
  lazy val totalDigitsValue: java.math.BigDecimal = getFacetValue(localTotalDigitsValue, Facet.totalDigits, hasTotalDigits)
  lazy val fractionDigitsValue: java.math.BigDecimal = getFacetValue(localFractionDigitsValue, Facet.fractionDigits, hasFractionDigits)

  private def errorOnLocalLessThanBaseFacet(local: Long, base: Long, theFacetType: Facet.Type) = {
    if (local < base) context.SDE("SimpleTypes: The local %s (%s) was less than the base %s (%s) ", theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalGreaterThanBaseFacet(local: Long, base: Long, theFacetType: Facet.Type) = {
    if (local > base) context.SDE("SimpleTypes: The local %s (%s) was greater than the base %s (%s) ", theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalLessThanBaseFacet(local: BigInteger,
    base: BigInteger, theFacetType: Facet.Type) = {
    val res = local.compareTo(base)
    if (res < 0) context.SDE("SimpleTypes: The local %s (%s) was less than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalGreaterThanBaseFacet(local: BigInteger,
    base: BigInteger, theFacetType: Facet.Type) = {
    val res = local.compareTo(base)
    if (res > 0) context.SDE("SimpleTypes: The local %s (%s) was greater than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalLessThanBaseFacet(local: java.math.BigDecimal,
    base: java.math.BigDecimal, theFacetType: Facet.Type) = {
    val res = local.compareTo(base)
    if (res < 0) context.SDE("SimpleTypes: The local %s (%s) was less than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalGreaterThanBaseFacet(local: java.math.BigDecimal,
    base: java.math.BigDecimal, theFacetType: Facet.Type) = {
    val res = local.compareTo(base)
    if (res > 0) context.SDE("SimpleTypes: The local %s (%s) was greater than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }

  private def getRemoteFacets(theFacetType: Facet.Type): Seq[FacetValueR] = {
    val remoteValues = remoteBaseFacets.filter { case (f, _) => f == theFacetType }
    if (remoteValues.size > 0) {
      val res: Seq[FacetValueR] = remoteValues.map { case (f, v) => (f, v.r) }
      res
    } else Seq.empty
  }

  private def getRemoteFacetValues(theFacetType: Facet.Type): Seq[FacetValue] = {
    val res = remoteBaseFacets.filter { case (f, _) => f == theFacetType }
    res
  }

  private def getRemoteFacetValue(theFacetType: Facet.Type): String = {
    // Filtering works more appropriately here
    val res = remoteBaseFacets.filter { case (f, v) => f == theFacetType }
    if (res.size > 0) {
      val (_, theFacetValue) = res(0)
      return theFacetValue
    }
    "" // Indicates the facet doesn't exist
  }

  private def evaluateFacet(check: (Long, Long, Facet.Type) => Unit, theFacetType: Facet.Type, theLocalFacet: Long) = {
    val remoteFacetValues = getRemoteFacetValues(theFacetType)
    if (remoteFacetValues.size > 0) {
      val (_, remoteValues) = remoteFacetValues(0)
      val theRemoteFacet = remoteValues(0).toLong
      check(theLocalFacet, theRemoteFacet, theFacetType)
    }
    theLocalFacet
  }

  private def getFacetValue(theLocalValue: String, theRemoteValue: String, theType: Facet.Type, exists: Boolean): java.math.BigDecimal = {
    if (!exists) context.SDE("The facet %s was not found.", theType)
    else if (theLocalValue != "" && theRemoteValue != "") {
      val resFacet = doNumericFacetNarrowing(theLocalValue, theRemoteValue, theType)
      new java.math.BigDecimal(resFacet)
    } else if (theLocalValue != "") {
      checkValueSpaceFacetRange(theLocalValue, theType)
    } else {
      checkValueSpaceFacetRange(theRemoteValue, theType)
    }
  }

  private def getFacetValue(theLocalValue: String, theType: Facet.Type, exists: Boolean): java.math.BigDecimal = {
    val remoteFacets = getRemoteFacetValues(theType)
    if (!exists) context.SDE("The facet %s was not found.", theType)
    else if (theLocalValue != "" && remoteFacets.size > 0) {
      val (_, remoteValue) = getRemoteFacetValues(theType)(0)
      val resFacet = doNumericFacetNarrowing(theLocalValue, remoteValue, theType)
      new java.math.BigDecimal(resFacet)
    } else if (theLocalValue != "") {
      checkValueSpaceFacetRange(theLocalValue, theType)
    } else {
      val (_, remoteValue) = remoteFacets(0)
      checkValueSpaceFacetRange(remoteValue, theType)
    }
  }

  private def narrowNonNegativeFacets(localFacet: String, remoteFacet: String, facetType: Facet.Type): String = {
    val theLocalFacet = new BigInteger(localFacet)
    val theRemoteFacet = new BigInteger(remoteFacet)
    if (theLocalFacet.signum() != 1) context.SDE("The %s facet must be a non-negative integer.", facetType)
    facetType match {
      case Facet.minLength => {
        errorOnLocalLessThanBaseFacet(theLocalFacet, theRemoteFacet, facetType)
        localFacet
      }
      case Facet.maxLength | Facet.fractionDigits => {
        errorOnLocalGreaterThanBaseFacet(theLocalFacet, theRemoteFacet, facetType)
        localFacet
      }
      case _ => {
        val errMsg = "narrowNonNegativeFacets is not valid for %s facet".format(facetType)
        Assert.usageError(errMsg)
      }
    }
  }

  private def narrowPositiveIntegerFacets(localFacet: String, remoteFacet: String, facetType: Facet.Type): String = {
    val theLocalFacet = new BigInteger(localFacet)
    val theRemoteFacet = new BigInteger(remoteFacet)
    if ((theLocalFacet.signum() != 1) || (theLocalFacet.compareTo(BigInteger.ZERO) == 0)) context.SDE("The %s facet must be a positive integer.", facetType)
    facetType match {
      case Facet.totalDigits => {
        errorOnLocalGreaterThanBaseFacet(theLocalFacet, theRemoteFacet, facetType)
        localFacet
      }
      case _ => {
        val errMsg = "narrowPositiveIntegerFacets is not valid for %s facet".format(facetType)
        Assert.usageError(errMsg)
      }
    }
  }

  private def narrowValueSpaceFacets(localFacet: String, remoteFacet: String, facetType: Facet.Type) = {
    val (theLocalFacet, theRemoteFacet) = checkValueSpaceFacetRange(localFacet, remoteFacet, facetType)

    // Made it here so range checks were successful
    // Now just validate/compare local and base/remote facet
    facetType match {
      case Facet.minInclusive => { errorOnLocalLessThanBaseFacet(theLocalFacet, theRemoteFacet, facetType) }
      case Facet.maxInclusive => { errorOnLocalGreaterThanBaseFacet(theLocalFacet, theRemoteFacet, facetType) }
      case Facet.minExclusive => { errorOnLocalLessThanBaseFacet(theLocalFacet, theRemoteFacet, facetType) }
      case Facet.maxExclusive => { errorOnLocalGreaterThanBaseFacet(theLocalFacet, theRemoteFacet, facetType) }
      case _ => {
        val errMsg = "Unrecognized facet type (%s) for narrowing of value-space facets.".format(facetType)
        Assert.usageError(errMsg)
      }
    }
    localFacet
  }

  private def convertFacetToBigDecimal(facet: String): java.math.BigDecimal = {
    self.primitiveType match {
      case PrimType.DateTime => dateToBigDecimal(facet, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx", PrimType.DateTime.toString(), context)
      case PrimType.Date => dateToBigDecimal(facet, "uuuu-MM-ddxxx", PrimType.Date.toString(), context)
      case PrimType.Time => dateToBigDecimal(facet, "HH:mm:ss.SSSSSSxxx", PrimType.Time.toString(), context)
      case _ => new java.math.BigDecimal(facet)
    }
  }

  private def checkValueSpaceFacetRange(localFacet: String, facetType: Facet.Type): java.math.BigDecimal = {
    // Necessary for min/max Inclusive/Exclusive Facets

    // Perform conversions once
    //val theLocalFacet = new java.math.BigDecimal(localFacet)
    val theLocalFacet = convertFacetToBigDecimal(localFacet)

    facetType match {
      case Facet.maxExclusive | Facet.maxInclusive |
        Facet.minExclusive | Facet.minInclusive | Facet.enumeration => {
        // Here we're just doing range checking for the
        // specified primitive type
        primitiveType match {
          case PrimType.Int => {
            if (!isFacetInIntRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Int range.",
                facetType, localFacet)
            }
          }
          case PrimType.Byte => {
            if (!isFacetInByteRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Byte range.",
                facetType, localFacet)
            }
          }
          case PrimType.Short => {
            if (!isFacetInShortRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Short range.",
                facetType, localFacet)
            }
          }
          case PrimType.Long => {
            if (!isFacetInLongRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Long range.",
                facetType, localFacet)
            }
          }
          case PrimType.Integer => {
            // Unbounded integer
            if (!isFacetInIntegerRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Integer range.",
                facetType, localFacet)
            }
          }
          case PrimType.UInt => {
            if (!isFacetInUnsignedIntRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of unsigned int range.",
                facetType, localFacet)
            }
          }
          case PrimType.UByte => {
            if (!isFacetInUnsignedByteRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of unsigned byte range.",
                facetType, localFacet)
            }
          }
          case PrimType.UShort => {
            if (!isFacetInUnsignedShortRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of unsigned short range.",
                facetType, localFacet)
            }
          }
          case PrimType.ULong => {
            if (!isFacetInUnsignedLongRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of unsigned long range.",
                facetType, localFacet)
            }
          }
          case PrimType.Double => {
            if (!isFacetInDoubleRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Double range.",
                facetType, localFacet)
            }
          }
          case PrimType.Float => {
            if (!isFacetInFloatRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of Float range.",
                facetType, localFacet)
            }
          }
          case PrimType.NonNegativeInteger => {
            // Unsigned Unbounded Integer
            if (!isFacetInNonNegativeIntegerRange(theLocalFacet)) {
              context.SDE("%s facet value (%s) was found to be outside of NonNegativeInteger range.",
                facetType, localFacet)
            }
          }
          case PrimType.DateTime => { /* Nothing to do here */ }
          case PrimType.Date => { /* Nothing to do here */ }
          case PrimType.Time => { /* Nothing to do here */ }
          case PrimType.Boolean => notYetImplemented("checkValueSpaceFacetRange - Boolean")
          case PrimType.HexBinary => { /* Nothing to do here */ }
          case PrimType.String => { /* Nothing to do here */ }
          case _ => schemaDefinitionError("checkValueSpaceFacetRange - Unrecognized primitive type: %s", primitiveType.name)
        }
      }
      case _ => { /* Nothing to do */ }
    }
    theLocalFacet
  }

  private def checkValueSpaceFacetRange(localFacet: String,
    remoteFacet: String, facetType: Facet.Type): (java.math.BigDecimal, java.math.BigDecimal) = {
    // Neccessary for min/max Inclusive/Exclusive Facets

    // TODO: I think the performance here can be improved.
    //
    // Consider storing the remoteBaseFacets as their actual evaluated values
    // rather than just as String.  This would prevent us from having to perform
    // the checkValueSpaceFacetRange on the remoteFacet here as it would've already
    // been done in the base. --TRW

    // Perform conversions once
    val theRemoteFacet = checkValueSpaceFacetRange(remoteFacet, facetType) //new java.math.BigDecimal(remoteFacet)
    val theLocalFacet = checkValueSpaceFacetRange(localFacet, facetType)

    (theLocalFacet, theRemoteFacet)
  }

  //  private def isNumInRange(num: java.math.BigDecimal, min: java.math.BigDecimal,
  //    max: java.math.BigDecimal): Boolean = {
  //    val checkMin = num.compareTo(min)
  //    if (checkMin < 0) { return false } // num less than min
  //    val checkMax = num.compareTo(max)
  //    if (checkMax > 0) { return false } // num greater than max
  //    true
  //  }
  private def isFacetInByteRange(facet: java.math.BigDecimal): Boolean = self.isInByteRange(facet)

  private def isFacetInShortRange(facet: java.math.BigDecimal): Boolean = self.isInShortRange(facet)
  private def isFacetInIntRange(facet: java.math.BigDecimal): Boolean = self.isInIntRange(facet)
  private def isFacetInIntegerRange(facet: java.math.BigDecimal): Boolean = self.isInIntegerRange(facet)
  private def isFacetInLongRange(facet: java.math.BigDecimal): Boolean = self.isInLongRange(facet)
  private def isFacetInDoubleRange(facet: java.math.BigDecimal): Boolean = self.isInDoubleRange(facet)
  private def isFacetInFloatRange(facet: java.math.BigDecimal): Boolean = self.isInFloatRange(facet)
  private def isFacetInDecimalRange(facet: java.math.BigDecimal): Boolean = {
    // BigDecimal is unbounded? So nothing outside of its range?
    true
  }
  private def isFacetInNegativeIntegerRange(facet: java.math.BigDecimal): Boolean = self.isInNonNegativeIntegerRange(facet)
  private def isFacetInNonNegativeIntegerRange(facet: java.math.BigDecimal): Boolean = self.isInNonNegativeIntegerRange(facet)
  private def isFacetInUnsignedLongRange(facet: java.math.BigDecimal): Boolean =
    isInUnsignedLongRange(facet)

  private def isFacetInUnsignedIntRange(facet: java.math.BigDecimal): Boolean =
    isInUnsignedIntRange(facet)

  private def isFacetInUnsignedShortRange(facet: java.math.BigDecimal): Boolean =
    isInUnsignedShortRange(facet)

  private def isFacetInUnsignedByteRange(facet: java.math.BigDecimal): Boolean =
    isInUnsignedByteRange(facet)

  protected def doNumericFacetNarrowing(localFacet: String, remoteFacet: String, facetType: Facet.Type) = {
    // Assumes both local and remote facets exist
    // Only for Numeric facets
    //
    // Can likely do narrowing checks here
    //
    // BigInt use compareTo
    //  a negative number, zero, or a positive number as this BigInteger is numerically less than, 
    //  equal to, or greater than o, which must be a BigInteger.
    facetType match {
      case Facet.minLength | Facet.maxLength | Facet.fractionDigits => {
        // Non-negative Integers.  BigInt
        narrowNonNegativeFacets(localFacet, remoteFacet, facetType)
      }
      case Facet.minInclusive | Facet.maxInclusive | Facet.minExclusive | Facet.maxExclusive => {
        // In value-space of base type.  BigDecimal?
        narrowValueSpaceFacets(localFacet, remoteFacet, facetType)
      }
      case Facet.totalDigits => {
        // Positive Integer (value greater than 0). BigInt
        narrowPositiveIntegerFacets(localFacet, remoteFacet, facetType)
      }
      case _ => Assert.usageError("Call to 'doNumericFacetNarrowing' only valid for Numeric Facets.")
    }
  }

  private def getLocalValue(theType: Facet.Type) = {
    val res = localBaseFacets.filter { case (f, v) => f == theType }
    if (res.length > 0) {
      val (_, theFacetValue) = res(0)
      theFacetValue
    } else ""
  }

  protected def getCombinedValue(theType: Facet.Type) = {
    val lValue = getLocalValue(theType)
    val rValue = getRemoteFacetValue(theType)
    val cValue = getFacetValue(lValue, rValue, theType, true)
    cValue
  }

  protected def getCombinedValueEnum = {
    val lValue = getLocalValue(Facet.enumeration)
    val rValue = getRemoteFacetValue(Facet.enumeration)
    lValue.foreach(e => {
      if (rValue.length() > 0 && !rValue.contains(e)) context.SDE("Local enumerations must be a subset of base enumerations.")
    })
    if (lValue.length() > 0) { lValue }
    else { rValue }
  }

}

object Facet extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object enumeration extends Type
  case object fractionDigits extends Type
  case object maxExclusive extends Type
  case object maxInclusive extends Type
  case object maxLength extends Type
  case object minExclusive extends Type
  case object minInclusive extends Type
  case object minLength extends Type
  case object pattern extends Type
  case object totalDigits extends Type
  case object whiteSpace extends Type
}

object FacetTypes {
  // These were defined to make life simpler
  // TODO: Should we modify these to also include the name of the simpleType?
  type Values = String
  type ValuesR = Regex
  type FacetValue = (Facet.Type, Values)
  type FacetValueR = (Facet.Type, ValuesR)
  type ElemFacets = Seq[FacetValue]
  type ElemFacetsR = Seq[FacetValueR]
}

class SimpleTypeNode(name: String, parent: SimpleTypeNode, childrenArg: => List[SimpleTypeNode]) {
  // Eliminated a var here. Doing functional graph construction now below.
  lazy val children = childrenArg
  lazy val isHead: Boolean = parent == null
  lazy val lcaseName = name.toLowerCase()

  // names in lower case
  lazy val parentList: List[String] = {
    if (isHead) {
      List.empty
    } else {
      lcaseName :: parent.parentList
    }
  }

  def doesParentListContain(typeName: String): Boolean = {
    val list = parentList.filter(n => n == typeName.toLowerCase())
    list.size > 0
  }
}

trait SimpleTypeDerivation {
  lazy val simpleTypes = buildStructure

  def getSimpleTypeNode(name: String) = {
    simpleTypes.find(stn => stn.lcaseName == name.toLowerCase())
  }

  def isXDerivedFromY(nameX: String, nameY: String): Boolean = {
    getSimpleTypeNode(nameX) match {
      case Some(stn) => {
        stn.doesParentListContain(nameY)
      }
      case None => false
    }
  }

  private def buildStructure = {
    // This is how you construct a graph in functional programming.
    // These structures are recursive, but it all works out in the end. 
    lazy val anySimpleType: SimpleTypeNode = new SimpleTypeNode("anySimpleType", null, List(string, float, double, decimal, boolean, hexBinary))
    lazy val string = new SimpleTypeNode("string", anySimpleType, Nil)
    lazy val float = new SimpleTypeNode("float", anySimpleType, Nil)
    lazy val double = new SimpleTypeNode("double", anySimpleType, Nil)
    lazy val decimal = new SimpleTypeNode("decimal", anySimpleType, List(integer))
    lazy val boolean = new SimpleTypeNode("boolean", anySimpleType, Nil)
    lazy val hexBinary = new SimpleTypeNode("hexBinary", anySimpleType, Nil)
    lazy val integer: SimpleTypeNode = new SimpleTypeNode("integer", decimal, List(long, nonNegativeInteger))
    lazy val long = new SimpleTypeNode("long", integer, List(int))
    lazy val nonNegativeInteger = new SimpleTypeNode("nonNegativeInteger", integer, List(unsignedLong))
    lazy val int: SimpleTypeNode = new SimpleTypeNode("int", long, List(short))
    lazy val short: SimpleTypeNode = new SimpleTypeNode("short", int, List(byte))
    lazy val byte = new SimpleTypeNode("byte", short, Nil)
    lazy val unsignedLong: SimpleTypeNode = new SimpleTypeNode("unsignedLong", nonNegativeInteger, List(unsignedInt))
    lazy val unsignedInt = new SimpleTypeNode("unsignedInt", unsignedLong, List(unsignedShort))
    lazy val unsignedShort: SimpleTypeNode = new SimpleTypeNode("unsignedShort", unsignedInt, List(unsignedByte))
    lazy val unsignedByte = new SimpleTypeNode("unsignedByte", unsignedShort, Nil)

    List(anySimpleType, string, float, double, decimal, boolean, hexBinary, integer, long,
      nonNegativeInteger, int, short, byte, unsignedLong, unsignedInt, unsignedShort, unsignedByte)
  }
}

abstract class SimpleTypeDefBase(xmlArg: Node, val parent: SchemaComponent)
  extends AnnotatedSchemaComponent(xmlArg, parent)
  with SimpleTypeBase
  with DFDLStatementMixin
  with Facets
  with TypeChecks
  with SimpleTypeDerivation
  with OverlapCheckMixin {

  requiredEvaluations(myBaseTypeList)

  lazy val bases: Seq[SimpleTypeDefBase] =
    myBaseDef match {
      case None => Nil
      case Some(st: SimpleTypeDefBase) => st +: st.bases
      case _ => Nil
    }

  lazy val sTypeNonDefault: Seq[ChainPropProvider] = bases.map { _.nonDefaultFormatChain }
  lazy val sTypeDefault: Seq[ChainPropProvider] = bases.map { _.defaultFormatChain }

  // want a QueueSet i.e., fifo order if iterated, but duplicates
  // kept out of the set. Will simulate by calling distinct.
  lazy val nonDefaultPropertySources = nonDefaultPropertySources_.value
  private val nonDefaultPropertySources_ = LV('nonDefaultPropertySources) {
    val seq = (this.nonDefaultFormatChain +: sTypeNonDefault).distinct
    checkNonOverlap(seq)
    seq
  }

  lazy val defaultPropertySources = defaultPropertySources_.value
  private val defaultPropertySources_ = LV('defaultPropertySources) {
    val seq = (this.defaultFormatChain +: sTypeDefault).distinct
    seq
  }

  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  // Returns name of base class in the form of QName
  //
  lazy val restrictionBase: String = {
    val rsb = xml \\ "restriction" \ "@base"
    if (rsb.length != 1) {
      context.SDE("Restriction base was not found.")
    }
    rsb.head.text
  }

  lazy val optPrimitiveType = {
    val (nsURI, localName) = baseTypeQName
    if (nsURI == XMLUtils.XSD_NAMESPACE) {
      // XSD namespace
      val prim = schemaDocument.schemaSet.getPrimitiveType(nsURI, localName)
      schemaDefinitionUnless(prim != None,
        "Type {%s}%s is not an XSD primitive type.", nsURI, localName)
      prim
    } else None
  }

  lazy val myBaseDef = myBaseType match {
    case st: SimpleTypeDefBase => Some(st)
    case _ => None
  }

  lazy val myBaseTypeFactory = {
    Assert.invariant(restrictionBase.length() != 0)
    val (nsURI, localName) = baseTypeQName
    Assert.invariant(optPrimitiveType == None)
    val factory = schemaDocument.schemaSet.getGlobalSimpleTypeDef(nsURI, localName)
    factory
  }

  /**
   * Follows all indirections to get you the ultimate primitive
   * built-in simple type that must underlie all simple types
   * eventually.
   */
  lazy val primitiveType = {
    myBaseType.primitiveType
  }

  lazy val baseTypeQName = XMLUtils.QName(xml, restrictionBase, schemaDocument)

  lazy val myBaseType: SimpleTypeBase = {
    optPrimitiveType match {
      case Some(pt) => pt
      case None => {
        val bt = myBaseTypeFactory.map { _.forDerivedType(this) }
        bt match {
          case None => schemaDefinitionError("No type found for base: " + baseTypeQName)
          case Some(bt) => bt
        }
      }
    }
  }

  lazy val myBaseTypeList = List(myBaseType)

  lazy val localBaseFacets: ElemFacets = {
    val myFacets: Queue[FacetValue] = Queue.empty // val not var - it's a mutable collection
    if (localPatternValue.length > 0) { myFacets.enqueue((Facet.pattern, localPatternValue)) }
    if (localMinLengthValue.length > 0) { myFacets.enqueue((Facet.minLength, localMinLengthValue)) }
    if (localMaxLengthValue.length > 0) { myFacets.enqueue((Facet.maxLength, localMaxLengthValue)) }
    if (localMinInclusiveValue.length > 0) { myFacets.enqueue((Facet.minInclusive, localMinInclusiveValue)) }
    if (localMaxInclusiveValue.length > 0) { myFacets.enqueue((Facet.maxInclusive, localMaxInclusiveValue)) }
    if (localMinExclusiveValue.length > 0) { myFacets.enqueue((Facet.minExclusive, localMinExclusiveValue)) }
    if (localMaxExclusiveValue.length > 0) { myFacets.enqueue((Facet.maxExclusive, localMaxExclusiveValue)) }
    if (localTotalDigitsValue.length > 0) { myFacets.enqueue((Facet.totalDigits, localTotalDigitsValue)) }
    if (localFractionDigitsValue.length > 0) { myFacets.enqueue((Facet.fractionDigits, localFractionDigitsValue)) }
    if (localEnumerationValue.length > 0) { myFacets.enqueue((Facet.enumeration, localEnumerationValue)) }

    val res: ElemFacets = myFacets.toSeq
    res
  }

  lazy val combinedBaseFacets: Seq[FacetValue] = {
    val localF = localBaseFacets
    val remoteF = remoteBaseFacets

    val combined: Queue[FacetValue] = Queue.empty

    if (hasEnumeration) {
      val enumVal = getCombinedValueEnum
      combined.enqueue((Facet.enumeration, enumVal))
    }
    if (hasPattern) {
      val lPattern = localBaseFacets.filter { case (f, v) => f == Facet.pattern }
      val rPattern = remoteBaseFacets.filter { case (f, v) => f == Facet.pattern }
      val cPattern = lPattern.union(rPattern)
      cPattern.foreach(x => combined.enqueue(x))
    }
    if (hasMinLength) {
      val cValue = getCombinedValue(Facet.minLength)
      combined.enqueue((Facet.minLength, cValue.toString()))
    }
    if (hasMaxLength) {
      val cValue = getCombinedValue(Facet.maxLength)
      combined.enqueue((Facet.maxLength, cValue.toString()))
    }
    if (hasMaxInclusive) {
      val cValue = getCombinedValue(Facet.maxInclusive)
      combined.enqueue((Facet.maxInclusive, cValue.toString()))
    }
    if (hasMaxExclusive) {
      val cValue = getCombinedValue(Facet.maxExclusive)
      combined.enqueue((Facet.maxExclusive, cValue.toString()))
    }
    if (hasMinInclusive) {
      val cValue = getCombinedValue(Facet.minInclusive)
      combined.enqueue((Facet.minInclusive, cValue.toString()))
    }
    if (hasMinExclusive) {
      val cValue = getCombinedValue(Facet.minExclusive)
      combined.enqueue((Facet.minExclusive, cValue.toString()))
    }
    if (hasTotalDigits) {
      val cValue = getCombinedValue(Facet.totalDigits)
      combined.enqueue((Facet.totalDigits, cValue.toString()))
    }
    if (hasFractionDigits) {
      val cValue = getCombinedValue(Facet.fractionDigits)
      combined.enqueue((Facet.fractionDigits, cValue.toString()))
    }
    combined.toSeq
  }

  lazy val remoteBaseFacets = remoteBaseFacets_.value
  private val remoteBaseFacets_ = LV('remoteBaseFacets) {
    myBaseType match {
      case gstd: GlobalSimpleTypeDef => gstd.combinedBaseFacets
      case prim: PrimitiveType => Nil
      case _ => Assert.impossible()
    }
  }

  /**
   * Combine our statements with those of our base def (if there is one)
   *
   * The order is important here. I.e., we FIRST put in each list those from our base. Then our own local ones.
   */
  lazy val statements: Seq[DFDLStatement] = myBaseDef.map { _.statements }.getOrElse(Nil) ++ localStatements
  // TODO: refactor into shared code for combining all the annotations in the resolved set of annotations 
  // for a particular annotation point, checking that there is only one format annotation, that 
  // asserts and discriminators are properly excluding each-other, etc.
  // Code should be sharable for many kinds of annotation points, perhaps specialized for groups, complex type
  // elements, and simple type elements.
  //
  // See JIRA issue DFDL-481
  lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    myBaseDef.map { _.newVariableInstanceStatements }.getOrElse(Nil) ++ localNewVariableInstanceStatements
  lazy val (discriminatorStatements, assertStatements) = checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)
  private lazy val combinedAsserts: Seq[DFDLAssert] = myBaseDef.map { _.assertStatements }.getOrElse(Nil) ++ localAssertStatements
  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] = myBaseDef.map { _.discriminatorStatements }.getOrElse(Nil) ++ localDiscriminatorStatements

  lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = myBaseDef.map { _.setVariableStatements }.getOrElse(Nil) ++ localSetVariableStatements
    checkDistinctVariableNames(combinedSvs)
  }

}

class LocalSimpleTypeDef(xmlArg: Node, parent: ElementBase)
  extends SimpleTypeDefBase(xmlArg, parent)
  with LocalComponentMixin {

  lazy val baseName = (xml \ "restriction" \ "@base").text
  lazy val baseType = {
    val res = if (baseName == "") None
    else notYetImplemented("local simpleType with base attribute.") // should go find the global simple type here
  }

}

/**
 * We need a schema document and such for unit testing, also our PrimitiveType
 * needs a dummy schema document also so that our invariant, that *everything*
 * has a schema document, schema, and schema set
 * holds true even when we're not building up a "real" schema.
 */
object Fakes {
  lazy val sch = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
    <xs:element name="fake2" type="tns:fakeCT"/>
    <xs:complexType name="fakeCT">
      <xs:sequence>
        <xs:group ref="tns:fakeGroup"/>
        <xs:element ref="tns:fake"/>
      </xs:sequence>
    </xs:complexType>
    <xs:group name="fakeGroup">
      <xs:choice>
        <xs:sequence/>
      </xs:choice>
    </xs:group>)
  val DummyPrimitiveFactory = null
  lazy val xsd_sset = new SchemaSet(DummyPrimitiveFactory, sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get.forRoot()
  lazy val fakeCT = fakeSD.getGlobalElementDecl("fake2").get.forRoot().typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.modelGroup.asInstanceOf[Sequence]
  lazy val Seq(fs1, fs2) = fakeSequence.groupMembers
  lazy val fakeGroupRef = fs1.asInstanceOf[GroupRef]

}

// Primitives are not "global" because they don't appear in any schema document
sealed abstract class PrimitiveType
  extends SchemaComponent(<primitive/>, null)
  with SimpleTypeBase
  with NamedMixin {

  /**
   * When class name is isomorphic to the type name, compute automatically.
   */
  lazy val pname = {
    val cname = Misc.getNameFromClass(this)
    val first = cname(0).toLower
    val rest = cname.substring(1)
    first + rest
  }
  override lazy val namespace = XMLUtils.XSD_NAMESPACE
  override lazy val prefix = "xsd"

  import PrimType._

  override lazy val enclosingComponent = None // Shouldn't be used anyway.
  override lazy val fileDescription = "" // no file, no file description

  lazy val primitiveType = this

  override def toString = "PrimitiveType(" + prettyName + ")"

  override lazy val name = pname
  override def prettyName = pname

  // override val xml = Assert.invariantFailed("Primitives don't have xml definitions.")

  override lazy val schemaDocument = Fakes.fakeSD
}

object PrimType {
  type Type = PrimitiveType
  case object String extends Type
  case object Int extends Type
  case object Byte extends Type
  case object Short extends Type
  case object Long extends Type
  case object Integer extends Type
  case object Decimal extends Type
  case object UInt extends Type { override lazy val pname = "unsignedInt" }
  case object UByte extends Type { override lazy val pname = "unsignedByte" }
  case object UShort extends Type { override lazy val pname = "unsignedShort" }
  case object ULong extends Type { override lazy val pname = "unsignedLong" }
  case object NonNegativeInteger extends Type
  case object Double extends Type
  case object Float extends Type
  case object HexBinary extends Type
  case object Boolean extends Type
  case object DateTime extends Type
  case object Date extends Type
  case object Time extends Type
  lazy val allPrimitiveTypes: Seq[PrimitiveType] = List(
    String,
    Int,
    Byte,
    Short,
    Long,
    Integer,
    Decimal,
    UInt,
    UByte,
    UShort,
    ULong,
    NonNegativeInteger,
    Double,
    Float,
    HexBinary,
    Boolean,
    DateTime,
    Date,
    Time)
}

/**
 * The factory is sharable even though the global object it creates cannot
 * be shared.
 *
 * Call forElement(element) and supply the element referring
 * to the global type, then you get back an instance that is one-to-one with the
 * element.
 *
 * This then allows attributes of the type to refer to the element in deciding things.
 * I.e., the context is clear and kept separate for each place a global type is used.
 */

class GlobalSimpleTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponent(xmlArg, schemaDocumentArg) with NamedMixin {
  // def forRoot() = new GlobalSimpleTypeDef(xml, schemaDocument, None)

  /**
   * Create a private instance for this element's use.
   */
  def forElement(element: ElementBase) = new GlobalSimpleTypeDef(None, xml, schemaDocument, Some(element))
  def forDerivedType(derivedType: SimpleTypeDefBase) = new GlobalSimpleTypeDef(Some(derivedType), xml, schemaDocument, None)

}
/**
 * The instance type for global simple type definitions.
 */

class GlobalSimpleTypeDef(derivedType: Option[SimpleTypeDefBase], xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: Option[ElementBase])
  extends SimpleTypeDefBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

  override lazy val enclosingComponent = (derivedType, element) match {
    case (Some(dt), None) => derivedType
    case (None, Some(elem)) => element
    case _ => Assert.impossible("SimpleType must either have a derivedType or an element. Not both.")
  }

  override def prettyName = "simpleType." + name

}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends SchemaComponent(xmlArg, parent)
  with TypeBase
  with ComplexTypeBaseGrammarMixin {

  requiredEvaluations(modelGroup)

  def element: ElementBase

  override lazy val enclosingComponent = Some(element)

  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml

  lazy val Seq(modelGroup) = {
    val s = smg.value
    schemaDefinitionUnless(s.length == 1, "A complex type must have exactly one model-group element child which is a sequence, choice, or group reference.")
    s
  }

  private val smg = LV('smg) {
    xmlChildren.flatMap {
      xmlChild =>
        {
          val g = GroupFactory(xmlChild, this, 1) // discards unwanted text nodes also.
          g
        }
    }
  }

  // provides needed polymorphism across unannotated complex types, and
  // the annotated objects.
  lazy val localAndFormatRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }

  lazy val isScannable: Boolean = {
    val parentElem: ElementBase = enclosingComponent.get
    val unScannableChildren = modelGroup.group.groupMembers.filterNot { child =>
      (child.knownEncodingCharset == parentElem.knownEncodingCharset) && child.isScannable
    }
    unScannableChildren.length == 0
  }

  lazy val alignmentValueInBits: Int = {
    val children = modelGroup.group.groupMembers.sortBy(m => -m.alignmentValueInBits)
    children.headOption match {
      case Some(child) => child.alignmentValueInBits
      case None => 0
    }
  }

}

class GlobalComplexTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponent(xmlArg, schemaDocumentArg) with NamedMixin {

  def forElement(element: ElementBase) = new GlobalComplexTypeDef(xml, schemaDocument, element)

}

class GlobalComplexTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

}

class LocalComplexTypeDef(xmlArg: Node, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, element)
  with LocalComponentMixin {
  //nothing
}
