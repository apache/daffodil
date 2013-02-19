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

import scala.xml._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.xml._
import scala.collection.mutable.Queue
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.util.TestUtils
import java.math.BigInteger
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarPatternKind
import com.ibm.icu.util.ULocale
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.text.DateFormat
import java.text.ParsePosition

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase
  extends DiagnosticsProviding

trait SimpleTypeBase
  extends TypeBase {
  def context: SchemaComponent
  def primitiveType: PrimitiveType
}

trait Facets { self: SimpleTypeDefBase =>
  import Facet._
  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  def retrieveFacetValueFromRestrictionBase(xml: Node, facetName: Facet): String = {
    val res = xml \\ "restriction" \ facetName.toString() \ "@value"
    if (res.length > 0) res.head.text else ""
  }
  def retrieveFacetValuesFromRestrictionBase(xml: Node, facetName: Facet): Seq[String] = {
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

  private def errorOnLocalLessThanBaseFacet(local: Long, base: Long, theFacetType: Facet) = {
    if (local < base) context.SDE("SimpleTypes: The local %s (%s) was less than the base %s (%s) ", theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalGreaterThanBaseFacet(local: Long, base: Long, theFacetType: Facet) = {
    if (local > base) context.SDE("SimpleTypes: The local %s (%s) was greater than the base %s (%s) ", theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalLessThanBaseFacet(local: BigInteger,
                                            base: BigInteger, theFacetType: Facet) = {
    val res = local.compareTo(base)
    if (res < 0) context.SDE("SimpleTypes: The local %s (%s) was less than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalGreaterThanBaseFacet(local: BigInteger,
                                               base: BigInteger, theFacetType: Facet) = {
    val res = local.compareTo(base)
    if (res > 0) context.SDE("SimpleTypes: The local %s (%s) was greater than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalLessThanBaseFacet(local: java.math.BigDecimal,
                                            base: java.math.BigDecimal, theFacetType: Facet) = {
    val res = local.compareTo(base)
    if (res < 0) context.SDE("SimpleTypes: The local %s (%s) was less than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }
  private def errorOnLocalGreaterThanBaseFacet(local: java.math.BigDecimal,
                                               base: java.math.BigDecimal, theFacetType: Facet) = {
    val res = local.compareTo(base)
    if (res > 0) context.SDE("SimpleTypes: The local %s (%s) was greater than the base %s (%s) ",
      theFacetType, local, theFacetType, base)
  }

  private def getRemoteFacets(theFacetType: Facet): Seq[FacetValueR] = {
    val remoteValues = remoteBaseFacets.filter { case (f, _) => f == theFacetType }
    if (remoteValues.size > 0) {
      val res: Seq[FacetValueR] = remoteValues.map { case (f, v) => (f, v.r) }
      res
    } else Seq.empty
  }

  private def getRemoteFacetValues(theFacetType: Facet): Seq[FacetValue] = {
    val res = remoteBaseFacets.filter { case (f, _) => f == theFacetType }
    res
  }

  private def getRemoteFacetValue(theFacetType: Facet): String = {
    // Filtering works more appropriately here
    val res = remoteBaseFacets.filter { case (f, v) => f == theFacetType }
    if (res.size > 0) {
      val (_, theFacetValue) = res(0)
      return theFacetValue
    }
    "" // Indicates the facet doesn't exist
  }

  private def evaluateFacet(check: (Long, Long, Facet) => Unit, theFacetType: Facet, theLocalFacet: Long) = {
    val remoteFacetValues = getRemoteFacetValues(theFacetType)
    if (remoteFacetValues.size > 0) {
      val (_, remoteValues) = remoteFacetValues(0)
      val theRemoteFacet = remoteValues(0).toLong
      check(theLocalFacet, theRemoteFacet, theFacetType)
    }
    theLocalFacet
  }

  private def getFacetValue(theLocalValue: String, theRemoteValue: String, theType: Facet, exists: Boolean): java.math.BigDecimal = {
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

  private def getFacetValue(theLocalValue: String, theType: Facet, exists: Boolean): java.math.BigDecimal = {
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

  private def narrowNonNegativeFacets(localFacet: String, remoteFacet: String, facetType: Facet): String = {
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

  private def narrowPositiveIntegerFacets(localFacet: String, remoteFacet: String, facetType: Facet): String = {
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

  private def narrowValueSpaceFacets(localFacet: String, remoteFacet: String, facetType: Facet) = {
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

  private def dateToBigDecimal(date: String, format: String, dateType: String): java.math.BigDecimal = {
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
        }catch {
          case e: Exception => context.SDE("Failed to parse (%s) to %s (%s)", date, dateType, format)
        }
        
      }
    }
    bd
  }

  private def convertFacetToBigDecimal(facet: String): java.math.BigDecimal = {
    self.primitiveType.myPrimitiveType match {
      case PrimType.DateTime => {
        // TODO: Fractional seconds or not?
        val f1 = "yyyy-MM-dd'T'HH:mm:ss.SSSZZZZZ"
        val f2 = "yyyy-MM-dd'T'HH:mm:ssZZZZZ"
        dateToBigDecimal(facet, f2, PrimType.DateTime.toString())
      }
      case PrimType.Date => dateToBigDecimal(facet, "yyyy-MM-dd", PrimType.Date.toString())
      case PrimType.Time => dateToBigDecimal(facet, "HH:mm:ssZZZZZ", PrimType.Time.toString())
      case _ => new java.math.BigDecimal(facet)
    }
  }

  private def checkValueSpaceFacetRange(localFacet: String, facetType: Facet): java.math.BigDecimal = {
    // Necessary for min/max Inclusive/Exclusive Facets

    // Perform conversions once
    //val theLocalFacet = new java.math.BigDecimal(localFacet)
    val theLocalFacet = convertFacetToBigDecimal(localFacet)

    facetType match {
      case Facet.maxExclusive | Facet.maxInclusive |
        Facet.minExclusive | Facet.minInclusive | Facet.enumeration => {
        // Here we're just doing range checking for the
        // specified primitive type
        primitiveType.myPrimitiveType match {
          case PrimType.Int => {
            if (!isFacetInIntegerRange(theLocalFacet)) {
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
          case PrimType.DateTime => { /* Nothing to do here */ }
          case PrimType.Date => { /* Nothing to do here */ }
          case PrimType.Time => { /* Nothing to do here */ }
          case PrimType.Boolean => Assert.notYetImplemented("checkValueSpaceFacetRange - Boolean")
          case PrimType.HexBinary => Assert.notYetImplemented("checkValueSpaceFacetRange - HexBinary")
          case _ => schemaDefinitionError("checkValueSpaceFacetRange - Unrecognized primitive type: %s", primitiveType.name)
        }
      }
      case _ => { /* Nothing to do */ }
    }
    theLocalFacet
  }

  private def checkValueSpaceFacetRange(localFacet: String,
                                        remoteFacet: String, facetType: Facet): (java.math.BigDecimal, java.math.BigDecimal) = {
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

  private def isNumInRange(num: java.math.BigDecimal, min: java.math.BigDecimal,
                           max: java.math.BigDecimal): Boolean = {
    val checkMin = num.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    val checkMax = num.compareTo(max)
    if (checkMax > 0) { return false } // num greater than max
    true
  }
  private def isFacetInByteRange(facet: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Byte.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Byte.MaxValue.toLong.toString())
    isNumInRange(facet, min, max)
  }
  private def isFacetInShortRange(facet: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Short.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Short.MaxValue.toLong.toString())
    isNumInRange(facet, min, max)
  }
  private def isFacetInIntegerRange(facet: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    isNumInRange(facet, min, max)
  }
  private def isFacetInLongRange(facet: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Long.MinValue.toString())
    val max = new java.math.BigDecimal(Long.MaxValue.toString())
    isNumInRange(facet, min, max)
  }
  private def isFacetInDoubleRange(facet: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Double.MinValue.toString())
    val max = new java.math.BigDecimal(Double.MaxValue.toString())
    isNumInRange(facet, min, max)
  }
  private def isFacetInFloatRange(facet: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Float.MinValue.toString())
    val max = new java.math.BigDecimal(Float.MaxValue.toString())
    isNumInRange(facet, min, max)
  }
  private def isFacetInDecimalRange(facet: java.math.BigDecimal): Boolean = {
    // BigDecimal is unbounded? So nothing outside of its range?
    true
  }
  private def isFacetInNegativeIntegerRange(facet: java.math.BigDecimal): Boolean = {
    // TODO: NegativeInteger not supported in DFDL v1.0
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    val isNegative = facet.signum == 1
    if (!isNegative) context.SDE("Expected a negative integer for this facet.")
    val checkMin = facet.compareTo(min)
    if (checkMin < 0) context.SDE("Facet value (%s) was found to be more negative than allowed by Int.MinValue.", facet.intValue())
    true
  }
  private def isFacetInNonNegativeIntegerRange(facet: java.math.BigDecimal): Boolean = {
    val min = java.math.BigDecimal.ZERO
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    val isNegative = facet.signum == 1
    if (isNegative) context.SDE("Expected a non-negative integer for this facet.")
    val checkMax = facet.compareTo(max)
    if (checkMax > 0) context.SDE("Facet value (%s) was found to be larger than Int.MaxValue.", facet.intValue())
    true
  }
  private def isFacetInUnsignedXXXRange(facet: java.math.BigDecimal, numBits: Int, typeName: String): Boolean = {
    Assert.usage(numBits <= 64, "isFacetInUnsignedXXXRange: numBits must be <= 64.")
    val min = java.math.BigDecimal.ZERO
    val max = new java.math.BigDecimal(BigInteger.ONE.shiftLeft(numBits))
    val isNegative = facet.signum == 1
    if (isNegative) context.SDE("Expected an unsigned %s for this facet.", typeName)
    val checkMax = facet.compareTo(max)
    if (checkMax > 0) context.SDE("Facet value (%s) was found to be larger than unsigned %s max value.", facet, typeName)
    true
  }
  private def isFacetInUnsignedLongRange(facet: java.math.BigDecimal): Boolean = {
    isFacetInUnsignedXXXRange(facet, 64, "long")
  }
  private def isFacetInUnsignedIntRange(facet: java.math.BigDecimal): Boolean = {
    isFacetInUnsignedXXXRange(facet, 32, "int")
  }
  private def isFacetInUnsignedShortRange(facet: java.math.BigDecimal): Boolean = {
    isFacetInUnsignedXXXRange(facet, 16, "short")
  }
  private def isFacetInUnsignedByteRange(facet: java.math.BigDecimal): Boolean = {
    isFacetInUnsignedXXXRange(facet, 8, "byte")
  }

  protected def doNumericFacetNarrowing(localFacet: String, remoteFacet: String, facetType: Facet) = {
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

  private def getLocalValue(theType: Facet) = {
    val res = localBaseFacets.filter { case (f, v) => f == theType }
    if (res.length > 0) {
      val (_, theFacetValue) = res(0)
      theFacetValue
    } else ""
  }

  protected def getCombinedValue(theType: Facet) = {
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

object Facet extends Enumeration {
  type Facet = Value
  val enumeration, fractionDigits, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, totalDigits, whiteSpace = Value
}

object FacetTypes {
  // These were defined to make life simpler
  // TODO: Should we modify these to also include the name of the simpleType?
  type Values = String
  type ValuesR = Regex
  type FacetValue = (Facet.Facet, Values)
  type FacetValueR = (Facet.Facet, ValuesR)
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
  extends AnnotatedSchemaComponent(xmlArg)
  with SimpleTypeBase
  with DFDLStatementMixin
  with Facets
  with SimpleTypeDerivation
  with OverlapCheckMixin {

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
  lazy val nonDefaultPropertySources = {
    val seq = (this.nonDefaultFormatChain +: sTypeNonDefault).distinct
    checkNonOverlap(seq)
    seq
  }

  lazy val defaultPropertySources = {
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
    if (rsb.length != 1){
      context.SDE("Restriction base was not found.")
    }
    rsb.head.text
  }

  lazy val myPrimitiveType = {
    val (nsURI, localName) = baseTypeQName
    if (nsURI == XMLUtils.XSD_NAMESPACE
      ||
      nsURI == XMLUtils.DFDL_SUBSET_NAMESPACE) { // tolerate use of this subset.
      // XSD namespace
      val prim = schemaDocument.schema.schemaSet.getPrimitiveType(localName)
      schemaDefinition(prim != None,
        "Type " + localName + " is not an XSD primitive type.")
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
    Assert.invariant(myPrimitiveType == None)
    val factory = schemaDocument.schema.schemaSet.getGlobalSimpleTypeDef(nsURI, localName)
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
    myPrimitiveType match {
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

  lazy val diagnosticChildren: DiagnosticsList = annotationObjs ++ myBaseTypeList

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
  private lazy val remoteBaseFacets_ = LV('remoteBaseFacets) {
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

  val enclosingComponent = {
    Some(parent)
  }

  lazy val baseName = (xml \ "restriction" \ "@base").text
  lazy val baseType = {
    val res = if (baseName == "") None
    else Assert.notYetImplemented() // should go find the global simple type here
  }

  lazy val prettyName = "simpleType." + baseName // Of(" + parent.name + ")"
}

/**
 * We need a schema document and such for unit testing, also our PrimitiveType
 * needs a dummy schema document also so that our invariant, that *everything*
 * has a schema document, schema, and schema set
 * holds true even when we're not building up a "real" schema.
 */
object Fakes {
  lazy val sch = TestUtils.dfdlTestSchema(
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
  lazy val xsd_sset = new SchemaSet(sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
  lazy val fakeSD = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = fakeSD.getGlobalElementDecl("fake").get.forRoot()
  lazy val fakeCT = fakeSD.getGlobalElementDecl("fake2").get.forRoot().typeDef.asInstanceOf[GlobalComplexTypeDef]
  lazy val fakeSequence = fakeCT.modelGroup.asInstanceOf[Sequence]
  lazy val Seq(fs1, fs2) = fakeSequence.groupMembers
  lazy val fakeGroupRef = fs1.asInstanceOf[GroupRef]

}

object PrimType extends Enumeration {
  type PrimType = Value
  val String, Int, Byte, Short, Long, Integer, UInt, UByte, UShort, ULong, Double, Float, HexBinary, Boolean, DateTime, Date, Time = Value
}

// Primitives are not "global" because they don't appear in any schema document
class PrimitiveType(name_ : String)
  extends SchemaComponent(<primitive/>)
  with SimpleTypeBase // use fake schema document
  with NamedAnnotationAndComponentMixin {

  import PrimType._

  val enclosingComponent = None // Shouldn't be used anyway.

  val primitiveType = this

  override def toString = "PrimitiveType(" + prettyName + ")"

  override lazy val name = name_
  override lazy val prettyName = name_

  lazy val diagnosticChildren: DiagnosticsList = Nil

  // override val xml = Assert.invariantFailed("Primitives don't have xml definitions.")

  override lazy val schemaDocument = Fakes.fakeSD

  lazy val myPrimitiveType: PrimType = {
    name match {
      case "string" => PrimType.String
      case "int" => PrimType.Int
      case "byte" => PrimType.Byte
      case "short" => PrimType.Short
      case "long" => PrimType.Long
      case "integer" => PrimType.Integer
      case "unsignedInt" => PrimType.UInt
      case "unsignedByte" => PrimType.UByte
      case "unsignedShort" => PrimType.UShort
      case "unsignedLong" => PrimType.ULong
      case "double" => PrimType.Double
      case "float" => PrimType.Float
      case "hexBinary" => Assert.notYetImplemented("PrimitiveType: hexBinary")
      case "boolean" => Assert.notYetImplemented("PrimitiveType: boolean")
      case "dateTime" => PrimType.DateTime
      case "date" => PrimType.Date
      case "time" => PrimType.Time
      case _ => schemaDefinitionError("Unrecognized primitive type: " + name)
    }
  }

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

class GlobalSimpleTypeDefFactory(val xml: Node, val schemaDocument: SchemaDocument)
  extends NamedAnnotationAndComponentMixin {
  // def forRoot() = new GlobalSimpleTypeDef(xml, schemaDocument, None)

  lazy val context = schemaDocument
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

  val enclosingComponent = (derivedType, element) match {
    case (Some(dt), None) => derivedType
    case (None, Some(elem)) => element
    case _ => Assert.impossible("SimpleType must either have a derivedType or an element. Not both.")
  }

  def schemaDocument = schemaDocumentArg

  override lazy val prettyName = "simpleType." + name

}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends SchemaComponent(xmlArg)
  with TypeBase
  with ComplexTypeBaseGrammarMixin {
  def element: ElementBase

  val enclosingComponent = Some(element)

  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml

  lazy val Seq(modelGroup) = smg.value
  lazy val smg = LV('smg) {
    xmlChildren.flatMap { GroupFactory(_, this, 1) }
  }

  // provides needed polymorphism across unannotated complex types, and
  // the annotated objects.
  lazy val localAndFormatRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }

  lazy val diagnosticChildren: DiagnosticsList = List(modelGroup)
}

class GlobalComplexTypeDefFactory(val xml: Node, val schemaDocument: SchemaDocument)
  extends NamedAnnotationAndComponentMixin {

  lazy val context = schemaDocument
  def forElement(element: ElementBase) = new GlobalComplexTypeDef(xml, schemaDocument, element)
}

class GlobalComplexTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

  def schemaDocument = schemaDocumentArg

  override lazy val prettyName = "complexType." + name
}

class LocalComplexTypeDef(xmlArg: Node, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, element)
  with LocalComponentMixin {

  lazy val prettyName = "complexType" // Of(" + element.name + ")"
}
