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

package edu.illinois.ncsa.daffodil.schema.annotation.props

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.util.Misc._
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.dsom.NotFound
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.dsom.PropertyLookupResult
import edu.illinois.ncsa.daffodil.dsom.LookupLocation
import java.net.URL
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation

/**
 * This test shows how to use the Generated Code mixins, and verifies that they work.
 */
class TestGeneratedProperties {

  val bagOfProps = <dfdl:format bitOrder="mostSignificantBitFirst" encoding="UTF-8" utf16Width="fixed" byteOrder="bigEndian" ignoreCase="no" outputNewLine="%CR;%LF;" alignment="1" alignmentUnits="bytes" fillByte="0" leadingSkip="0" trailingSkip="0" lengthKind="delimited" lengthUnits="characters" prefixIncludesPrefixLength="no" representation="text" textPadKind="none" textTrimKind="none" escapeSchemeRef="tns:GeneralBlockEscapeScheme" textBidi="no" textBidiTextOrdering="implicit" textBidiSymmetric="yes" textBidiTextShaped="no" textBidiNumeralShapes="nominal" textBidiOrientation="RTL" textStringJustification="left" textStringPadCharacter="%SP;" truncateSpecifiedLengthString="no" textOutputMinLength="0" textNumberJustification="right" textNumberPadCharacter="0" decimalSigned="yes" textNumberCheckPolicy="lax" textNumberRep="standard" textStandardBase="10" textNumberRounding="pattern" textNumberRoundingMode="roundUp" textNumberRoundingIncrement="0.0" textStandardDecimalSeparator="." textStandardGroupingSeparator="," textStandardExponentRep="E" textStandardZeroRep="0" textStandardInfinityRep="Inf" textStandardNaNRep="NaN" textNumberPattern="#0" textZonedSignStyle="asciiStandard" textBooleanJustification="left" textBooleanPadCharacter="%SP;" textBooleanTrueRep="true" textBooleanFalseRep="false" textCalendarJustification="left" textCalendarPadCharacter="%SP;" calendarPatternKind="implicit" calendarPattern="yyyy-MM-dd'T'HH:mm:ss" calendarCheckPolicy="lax" calendarTimeZone="UTC" calendarObserveDST="yes" calendarFirstDayOfWeek="Monday" calendarDaysInFirstWeek="4" calendarCenturyStart="53" calendarLanguage="en-US" occursCountKind="parsed" sequenceKind="ordered" separator="," separatorSuppressionPolicy="never" separatorPosition="infix" initiatedContent="no" floating="no" choiceLengthKind="implicit" initiator="" terminator="" documentFinalTerminatorCanBeMissing="no" emptyValueDelimiterPolicy="none" nilKind="literalValue" useNilForDefault="no" nilValue="NIL" nilValueDelimiterPolicy="none" binaryNumberRep="binary" binaryPackedSignCodes="C D F C" binaryDecimalVirtualPoint="0" binaryNumberCheckPolicy="lax" binaryFloatRep="ieee" binaryCalendarRep="bcd" binaryCalendarEpoch="1970-01-01T00:00:00+00:00" binaryBooleanTrueRep="1" binaryBooleanFalseRep="0">
                   </dfdl:format>

  /**
   * To use the Generated Code Mixins, you mix one in like so, then
   * you have to define the getPropertyOption method, which actually
   * retrieves the property if it is present.
   */
  class HasLotsOfProperties
    extends SchemaComponentBase(<dummy/>, null)
    with LookupLocation
    with Format_AnnotationMixin
    with SeparatorSuppressionPolicyMixin {

    def columnAttribute: Option[String] = ???
    def fileAttribute: Option[String] = ???
    def lineAttribute: Option[String] = ???
    def columnDescription: String = ???
    def fileDescription: String = ???
    def lineDescription: String = ???
    def locationDescription: String = ???
    def namespaces: scala.xml.NamespaceBinding = ???
    def schemaFileLocation: SchemaFileLocation = ???

    override lazy val oolagContext = this
    override val xml = bagOfProps
    lazy val fileName = "file:dummy"
    lazy val properties: PropMap = Map.empty

    def SDE(id: String, args: Any*): Nothing = {
      throw new Exception(id.format(args: _*))
    }

    def SDEButContinue(id: String, args: Any*): Unit = {
      System.err.println(new Exception(id.format(args: _*)))
    }

    def SDW(id: String, args: Any*): Unit = {
      System.err.println(new Exception(id.format(args: _*)))
    }

    /**
     * In this case, we're just going to lookup the attribute on the above
     * big piece of XML. In real use, this would look at some list built up
     * by using the DFDL property scoping rules to have the right set of property
     * definitions in it.
     */
    def findPropertyOption(pname: String): PropertyLookupResult = {
      val propNodeSeq = bagOfProps.attribute(pname)
      propNodeSeq match {
        case None => NotFound(Seq(this), Nil) // attribute was not found
        case Some(nodeseq) => {
          //
          // Interesting that attributeName="" produces a Nil nodeseq, not an empty string.
          // 
          // This whole attributes as NodeSeq thing in Scala seems strange, but attributes
          // can contain unresolved entities, e.g., quote="&amp;quot;2B || ! 2B&amp;quot;"
          // so really they do have to return them as node sequences. It requires DTD processing
          // to resolve everything, and most code isn't going to process the DTDs. I.e., the scala 
          // XML library lets your code be the one doing the DTD resolving, so they can't do it for you.
          //
          nodeseq match {
            case Nil => Found("", this) // we want to hand back the empty string as a value.
            case _ => Found(nodeseq.toString, this)
          }
        }
      }
    }
  }

  def comparePropValue(prop: Any, value: String) = {
    if (prop.isInstanceOf[EnumValueBase]) {
      val withInitialUpcase = initialUpperCase(value)
      assertEquals(withInitialUpcase, prop.toString)
    } else
      assertEquals(value, prop.toString)
  }

  @Test
  def testProps1() {
    val hasProps = new HasLotsOfProperties

    //    comparePropValue(hasProps.encoding, "UTF-8")
    comparePropValue(hasProps.utf16Width, "fixed")
    //    comparePropValue(hasProps.byteOrder, "bigEndian")
    comparePropValue(hasProps.ignoreCase, "no")
    //    comparePropValue(hasProps.outputNewLine, "%CR;%LF;")
    comparePropValue(hasProps.alignment, "1")
    comparePropValue(hasProps.alignmentUnits, "bytes")
    comparePropValue(hasProps.fillByte, "0")
    comparePropValue(hasProps.leadingSkip, "0")
    comparePropValue(hasProps.trailingSkip, "0")
    comparePropValue(hasProps.lengthKind, "delimited")
    comparePropValue(hasProps.lengthUnits, "characters")
    comparePropValue(hasProps.prefixIncludesPrefixLength, "no")
    comparePropValue(hasProps.representation, "text")
    comparePropValue(hasProps.textPadKind, "none")
    comparePropValue(hasProps.textTrimKind, "none")
    comparePropValue(hasProps.escapeSchemeRef, "tns:GeneralBlockEscapeScheme")
    comparePropValue(hasProps.textBidi, "no")
    comparePropValue(hasProps.textBidiTextOrdering, "implicit")
    comparePropValue(hasProps.textBidiSymmetric, "yes")
    comparePropValue(hasProps.textBidiTextShaped, "no")
    comparePropValue(hasProps.textBidiNumeralShapes, "nominal")
    comparePropValue(hasProps.textBidiOrientation, "RTL")
    comparePropValue(hasProps.textStringJustification, "left")
    //comparePropValue(hasProps.textStringPadCharacter, "%SP;")
    comparePropValue(hasProps.truncateSpecifiedLengthString, "no")
    comparePropValue(hasProps.textOutputMinLength, "0")
    comparePropValue(hasProps.textNumberJustification, "right")
    //    comparePropValue(hasProps.textNumberPadCharacter, "0")
    comparePropValue(hasProps.decimalSigned, "yes")
    comparePropValue(hasProps.textNumberCheckPolicy, "lax")
    comparePropValue(hasProps.textNumberRep, "standard")
    comparePropValue(hasProps.textStandardBase, "10")
    comparePropValue(hasProps.textNumberRounding, "pattern")
    comparePropValue(hasProps.textNumberRoundingMode, "roundUp")
    comparePropValue(hasProps.textNumberRoundingIncrement, "0.0")
    //    comparePropValue(hasProps.textStandardDecimalSeparator, ".")
    //    comparePropValue(hasProps.textStandardGroupingSeparator, ",")
    //comparePropValue(hasProps.textStandardExponentRep, "E")
    //    comparePropValue(hasProps.textStandardZeroRep, "0")
    //    comparePropValue(hasProps.textStandardInfinityRep, "Inf")
    //    comparePropValue(hasProps.textStandardNaNRep, "NaN")
    comparePropValue(hasProps.textNumberPattern, "#0")
    comparePropValue(hasProps.textZonedSignStyle, "asciiStandard")
    comparePropValue(hasProps.textBooleanJustification, "left")
    //    comparePropValue(hasProps.textBooleanPadCharacter, "%SP;")
    //    comparePropValue(hasProps.textBooleanTrueRep, "true")
    //    comparePropValue(hasProps.textBooleanFalseRep, "false")
    comparePropValue(hasProps.textCalendarJustification, "left")
    //    comparePropValue(hasProps.textCalendarPadCharacter, "%SP;")
    comparePropValue(hasProps.calendarPatternKind, "implicit")
    comparePropValue(hasProps.calendarPattern, "yyyy-MM-dd'T'HH:mm:ss")
    comparePropValue(hasProps.calendarCheckPolicy, "lax")
    comparePropValue(hasProps.calendarTimeZone, "UTC")
    comparePropValue(hasProps.calendarObserveDST, "yes")
    comparePropValue(hasProps.calendarFirstDayOfWeek, "Monday")
    comparePropValue(hasProps.calendarDaysInFirstWeek, "4")
    comparePropValue(hasProps.calendarCenturyStart, "53")
    comparePropValue(hasProps.calendarLanguage, "en-US")
    comparePropValue(hasProps.occursCountKind, "parsed")
    comparePropValue(hasProps.sequenceKind, "ordered")
    //    comparePropValue(hasProps.separator, ",")
    comparePropValue(hasProps.separatorSuppressionPolicy, "never")
    comparePropValue(hasProps.separatorPosition, "infix")
    comparePropValue(hasProps.initiatedContent, "no")
    comparePropValue(hasProps.floating, "no")
    comparePropValue(hasProps.choiceLengthKind, "implicit")
    //    comparePropValue(hasProps.initiator, "")
    //    comparePropValue(hasProps.terminator, "")
    comparePropValue(hasProps.documentFinalTerminatorCanBeMissing, "no")
    comparePropValue(hasProps.emptyValueDelimiterPolicy, "none")
    comparePropValue(hasProps.nilKind, "literalValue")
    comparePropValue(hasProps.useNilForDefault, "no")
    //    comparePropValue(hasProps.nilValue, "NIL")
    comparePropValue(hasProps.nilValueDelimiterPolicy, "none")
    comparePropValue(hasProps.binaryNumberRep, "binary")
    comparePropValue(hasProps.binaryPackedSignCodes, "C D F C")
    comparePropValue(hasProps.binaryDecimalVirtualPoint, "0")
    comparePropValue(hasProps.binaryNumberCheckPolicy, "lax")
    //    comparePropValue(hasProps.binaryFloatRep, "ieee")
    comparePropValue(hasProps.binaryCalendarRep, "bcd")
    comparePropValue(hasProps.binaryCalendarEpoch, "1970-01-01T00:00:00+00:00")
    comparePropValue(hasProps.binaryBooleanTrueRep, "1")
    comparePropValue(hasProps.binaryBooleanFalseRep, "0")

    comparePropValue(hasProps.bitOrder, "mostSignificantBitFirst")

    // TODO: verify that toString prints all the properties and their values.
  }

  /**
   * Note that toString forces the evaluation of every property that has been
   * mixed into the object. (This is good for debugging.)
   *
   * If a property is not present; however, then it will not error. It will
   * print the properties that are defined for the object. I.e., suppose you
   * mixed in property Foobar, but the getPropertiesOption doesn't find a
   * property definition foobar="whatever" in whatever it looks in for those.
   *
   * In that case, toString just won't print anything for foobar. So the fact
   * that it is mixed in, but not defined, isn't an error (nor should it be).
   *
   * In other words, it is safe to mix in property mixins, for things you
   * don't need, and won't use. If the code doesn't ask for them, it won't cause
   * any error. Conversely, if you do ask for them, and it's not defined,
   * that's a schema definition error always.
   */
  @Test
  def testPropsToString() {
    val h = new HasLotsOfProperties
    h.ignoreCase
    val fl = h.toStringFunctionList
    // System.err.println("There are " + fl.length + " toString functions.")
    assertTrue(fl.length >= 85) // Note: There are extra print functions for things that won't be requested. like "ref". 
    // These tests don't work because we no longer make the printed rep of a object contain a listing of all its props.
    //    val str = h.toString
    //    println(str)
    //    assertTrue(str.contains("binaryCalendarRep=\"bcd\""))
    //    assertTrue(str.contains("binaryBooleanFalseRep='0'")) 
    //    assertTrue(str.contains("textBooleanPadCharacter='%SP;'"))
  }
}
