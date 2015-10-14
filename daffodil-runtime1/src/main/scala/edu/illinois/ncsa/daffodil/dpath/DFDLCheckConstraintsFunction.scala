/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dpath

// TODO: replace these with our own Infoset implementation objects

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LogLevel
import scala.xml.NamespaceBinding
import javax.xml.XMLConstants
import java.util.HashMap
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.Implicits._; object NoWarn { ImplicitsSuppressUnusedImportWarning() }
import com.ibm.icu.text.DateFormat
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.GregorianCalendar
import scala.xml.NodeSeq
import scala.math.BigDecimal
import org.w3c.dom.NodeList
import javax.xml.xpath.XPathFunction
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

object DFDLCheckConstraintsFunction {
  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._
  import util.control.Breaks._
  /**
   * Used for validation purposes when ValidationMode is Limited or Full.
   *
   * Provides the result Unit on Success or a message (String) on Failure.
   *
   * @param pstate the state containing the currentElement, data, primitiveType and context.
   *
   * @return a Boolean on success, String (message) on failure.
   */
  def validate(pstate: PState): Either[String, Unit] = {
    executeCheck(pstate.infoset.asInstanceOf[DISimple])
  }

  /**
   * Performs the constraint checks using information contained within the
   * PState object.
   *
   * @param pstate the current parser state.
   *
   * @return a Unit on success, String (message) on failure.
   */

  def executeCheck(currentElement: DISimple): Either[String, Unit] = {
    val e = currentElement.erd

    val data =
      if (!currentElement.isNilled) currentElement.dataValueAsString
      else ""

    def primType = e.optPrimType.get

    // We have an ElementBase, retrieve the constraints
    e.patternValues.foreach { patterns =>
      if (!currentElement.isNilled && patterns.size > 0) {
        val check = checkPatterns(data, patterns)
        if (!check) {
          val patternStrings = patterns.map { case (_, pattern) => pattern }.mkString(",")
          return Left("facet pattern(s): %s".format(patternStrings))
        }
      }
    }

    e.enumerationValues.foreach { enumerations =>
      if (!currentElement.isNilled && enumerations.size > 0) {
        val check = checkEnumerations(data, enumerations)
        if (!check) {
          return Left("facet enumeration(s): %s".format(enumerations.mkString(",")))
        }
      }
    }

    // Check minLength
    e.minLength.foreach { minLength =>
      val isMinLengthGreaterThanEqToZero = minLength.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNilled && isMinLengthGreaterThanEqToZero) {
        if (!checkMinLength(data, minLength, e, primType))
          return Left("facet minLength (%s)".format(minLength))
      }
    }
    // Check maxLength
    e.maxLength.foreach { maxLength =>
      val isMaxLengthGreaterThanEqToZero = maxLength.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNilled && isMaxLengthGreaterThanEqToZero) {
        if (!checkMaxLength(data, maxLength, e, primType))
          return Left("facet maxLength (%s)".format(maxLength))
      }
    }
    // Check minInclusive
    e.minInclusive.foreach { minInclusive =>
      if (!currentElement.isNilled) {
        if (!checkMinInc(data, minInclusive, primType, e))
          return Left("facet minInclusive (%s)".format(minInclusive))
      }
    }
    // Check maxInclusive
    e.maxInclusive.foreach { maxInclusive =>
      if (!currentElement.isNilled) {
        if (!checkMaxInc(data, maxInclusive, primType, e))
          return Left("facet maxInclusive (%s)".format(maxInclusive))
      }
    }
    // Check minExclusive
    e.minExclusive.foreach { minExclusive =>
      if (!currentElement.isNilled) {
        if (!checkMinExc(data, minExclusive, primType, e))
          return Left("facet minExclusive (%s)".format(minExclusive))
      }
    }
    // Check maxExclusive
    e.maxExclusive.foreach { maxExclusive =>
      if (!currentElement.isNilled) {
        if (!checkMaxExc(data, maxExclusive, primType, e))
          return Left("facet maxExclusive (%s)".format(maxExclusive))
      }
    }
    // Check totalDigits
    e.totalDigits.foreach { totalDigits =>
      val isTotalDigitsGreaterThanEqToZero = totalDigits.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNilled && isTotalDigitsGreaterThanEqToZero) {
        if (!checkTotalDigits(data, totalDigits))
          return Left("facet totalDigits (%s)".format(totalDigits))
      }
    }
    // Check fractionDigits
    e.fractionDigits.foreach { fractionDigits =>
      val isFractionDigitsGreaterThanEqToZero = fractionDigits.compareTo(java.math.BigDecimal.ZERO) >= 0
      if (!currentElement.isNilled && isFractionDigitsGreaterThanEqToZero) {
        if (!checkFractionDigits(data, fractionDigits))
          return Left("facet fractionDigits (%s)".format(fractionDigits))
      }
    }

    // Note: dont check occurs counts // if(!checkMinMaxOccurs(e, pstate.arrayPos)) { return java.lang.Boolean.FALSE }
    Right(())
  }

  def checkMinLength(data: String, minValue: java.math.BigDecimal,
    e: ElementRuntimeData, primType: PrimType): java.lang.Boolean = {
    primType match {
      case PrimType.String => {
        val bdData = new java.math.BigDecimal(data.length())
        val isDataLengthLess = bdData.compareTo(minValue) < 0
        if (isDataLengthLess) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        // Has to come through as a string in infoset
        // hex string is exactly twice as long as number of bytes
        // take length / 2 = length
        val bdDataLength = new java.math.BigDecimal(data.length() / 2)
        val isDataLengthEqual = bdDataLength.compareTo(minValue) == 0
        if (isDataLengthEqual) java.lang.Boolean.TRUE
        else java.lang.Boolean.FALSE
      }
      case _ => e.SDE("MinLength facet is only valid for string and hexBinary.")
    }
  }

  def checkMaxLength(data: String, maxValue: java.math.BigDecimal,
    e: ElementRuntimeData, primType: PrimType): java.lang.Boolean = {
    primType match {
      case PrimType.String => {
        val bdData = new java.math.BigDecimal(data.length())
        val isDataLengthGreater = bdData.compareTo(maxValue) > 0
        if (isDataLengthGreater) java.lang.Boolean.FALSE
        else java.lang.Boolean.TRUE
      }
      case PrimType.HexBinary => {
        // Has to come through as a string in infoset
        // hex string is exactly twice as long as number of bytes
        // take length / 2 = length
        val bdDataLength = new java.math.BigDecimal(data.length() / 2)
        val isDataLengthEqual = bdDataLength.compareTo(maxValue) == 0
        if (isDataLengthEqual) java.lang.Boolean.TRUE
        else java.lang.Boolean.FALSE
      }
      case _ => e.SDE("MaxLength facet is only valid for string and hexBinary.")
    }

  }

  // TODO: Duplication of dateToBigDecimal in Types.scala, throw in a library?
  // Note there is also similar code in Conversions.scala used by the DPath
  // runtime - lots of overlap between that and facet-checking code.
  def dateToBigDecimal(date: String, format: String, eb: ElementRuntimeData): java.math.BigDecimal = {
    val df = new SimpleDateFormat(format)
    df.setCalendar(new GregorianCalendar())
    df.setTimeZone(TimeZone.GMT_ZONE)
    val dt = try {
      df.parse(date)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e: Exception => eb.SDE("Failed to parse date (%s) to format (%s) due to %s.", date, format, e.getMessage())
    }
    if (dt == null) eb.SDE("Failed to parse date (%s) to format (%s).", date, format)
    new java.math.BigDecimal(dt.getTime())
  }

  // TODO: Duplication of convertFacetToBigDecimal in Types.scala , throw in a library?
  // Note there is also similar code in Conversions.scala used by the DPath
  // runtime - lots of overlap between that and facet-checking code.
  def convertDataToBigDecimal(data: String, primType: PrimType, e: ElementRuntimeData): java.math.BigDecimal = {
    primType match {
      case PrimType.DateTime => dateToBigDecimal(data, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx", e)
      case PrimType.Date => dateToBigDecimal(data, "uuuu-MM-ddxxx", e)
      case PrimType.Time => dateToBigDecimal(data, "HH:mm:ss.SSSSSSxxx", e)
      case _ => new java.math.BigDecimal(data)
    }
  }

  def checkMinInc(data: String, minValue: java.math.BigDecimal, primType: PrimType, e: ElementRuntimeData): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataGreaterThanEqToMinInc = bdData.compareTo(minValue) >= 0
    isDataGreaterThanEqToMinInc
  }

  def checkMinExc(data: String, minValue: java.math.BigDecimal, primType: PrimType, e: ElementRuntimeData): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataGreaterThanEqToMinExc = bdData.compareTo(minValue) > 0
    isDataGreaterThanEqToMinExc
  }

  def checkMaxInc(data: String, maxValue: java.math.BigDecimal, primType: PrimType, e: ElementRuntimeData): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataLessThanEqToMaxInc = bdData.compareTo(maxValue) <= 0
    isDataLessThanEqToMaxInc
  }

  def checkMaxExc(data: String, maxValue: java.math.BigDecimal, primType: PrimType, e: ElementRuntimeData): Boolean = {
    val bdData = convertDataToBigDecimal(data, primType, e)
    val isDataLessThanMaxExc = bdData.compareTo(maxValue) < 0
    isDataLessThanMaxExc
  }

  def checkTotalDigits(data: String, digits: java.math.BigDecimal): Boolean = {
    // Per http://www.w3.org/TR/xmlschema-2/#rf-totalDigits
    // |i| < 10^totalDigits
    val number = new java.math.BigDecimal(scala.math.pow(10.0, digits.doubleValue()))
    val biNumber = new java.math.BigInteger(number.intValueExact().toString())
    val bdData = new java.math.BigDecimal(data).unscaledValue()
    val isDataLessThanNumber = bdData.compareTo(biNumber) < 0
    isDataLessThanNumber
  }

  def checkFractionDigits(data: String, digits: java.math.BigDecimal): Boolean = {
    val bdData = new java.math.BigDecimal(data)
    // Rounding HALF_DOWN prevents us from accidentally increasing the value.
    val rounded = bdData.setScale(digits.intValue(), java.math.RoundingMode.HALF_DOWN)
    val isDataSameAsRounded = bdData.compareTo(rounded) == 0
    isDataSameAsRounded
  }

  def checkEnumerations(data: String, enumerations: String): Boolean = {
    data.matches(enumerations)
  }

  def checkPatterns(data: String, patterns: Seq[FacetValueR]): Boolean = {
    var isSuccess: Boolean = true

    breakable {
      for (simpleType <- patterns) {
        // each pattern within simpleType is OR'd
        // each pattern between simpleType's is AND'd

        // Each elem represents a simpleType
        // each simpleType is allowed a facetPattern
        // each facetPattern represents all patterns on this particular
        // simpleType.
        //
        // Ex.
        // <SimpleType name="A">
        //   <restriction base="B">
        //     <pattern value="1"/>
        //     <pattern value="2"/>
        //   </restriction>
        // </SimpleType>
        //
        // <SimpleType name="B">
        //   <restriction base="int">
        //     <pattern value="3"/>
        //     <pattern value="4"/>
        //   </restriction>
        // </SimpleType>
        //
        // Here facetPattern for SimpleType-A = "1|2" (OR'd)
        val (_ /* facetName */ , facetPattern) = simpleType

        // All patterns between simpleTypes must match (AND'd)
        if (!data.matches(facetPattern.toString())) {
          isSuccess = false
          break
        }
      }
    }
    return isSuccess
  }

  def checkOccurrance(minOccurs: Int, maxOccurs: Int, position: Long): Boolean = {
    // A maxOccurs of -1 signifies unbounded
    if ( // position > minOccurs && // DON"T CHECK MIN OCCURS.
    // That can't work. If minOccurs is 5 the first element at position 1 will fail this check.
    ((position <= maxOccurs) || (maxOccurs == -1))) { return true }
    return false
  }
}
