package edu.illinois.ncsa.daffodil.tdml

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

import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility
import scala.xml.XML
import edu.illinois.ncsa.daffodil.Implicits.using
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.assertFalse
import junit.framework.Assert.fail
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._

class TestTDMLRunnerNew {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  /**
   * Validation=Off
   * Should Parse Succeed? Yes
   * Exception expected? Yes
   *
   * Reasoning: The data parses successfully and validation is 'off'.
   * Demonstrates that when validation is off, no validation errors
   * should be expected by the testcase.
   */
  @Test def testValidationOffValidationErrorGivenShouldError() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testValidation" root="array" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:validationErrors>
            <tdml:error>Specifying this should throw exception</tdml:error>
          </tdml:validationErrors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("testValidation")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Test case invalid"))
    assertTrue(msg.contains("Validation is off"))
    assertTrue(msg.contains("test expects an error"))
  }

  /**
   * Validation=Off
   * Should Parse Succeed? Yes
   * Exception expected? No
   *
   * Reasoning: The data parses successfully and validation is 'off'.
   * Helps demonstrate the optionality of including the validation errors
   * in the testcase. <verrors/> means no validation errors are expected.
   */
  @Test def testValidationOffValidationErrorGivenButEmptyNotError() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testValidation" root="array" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:validationErrors/>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testValidation")
  }

  /**
   * Validation=Off
   * Should Parse Succeed? Yes
   * Exception expected? No
   *
   * Reasoning: The data parses successfully and validation is 'off'.
   * Helps demonstrate the optionality of including the validation errors
   * in the testcase.
   */
  @Test def testValidationOffValidationErrorNotGivenNotError() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testValidation" root="array" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testValidation")
  }

  /**
   * Validation=Limited
   * Should Parse Succeed? Yes
   * Exception expected? Yes
   *
   * Reasoning: The data parses successfully and fails 'limited' validation.
   * However the test case itself does not expect a validation error.  The
   * purpose is to alert the test writer to the fact that a validation occurred
   * that was not 'captured' in the test case.
   */
  @Test def testValidationLimitedValidationErrorNotCapturedShouldThrow() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testValidation" root="array" model="mySchema" validation="limited">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("testValidation")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Validation errors found where none were expected"))
  }

  /**
   * Test to make sure we can use freeform regex and also use the comment syntax.
   */
  @Test def testRegexWithFreeFormAndComments1() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="pattern" dfdl:terminator="abcdef">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:element>
                  <dfdl:property name="lengthPattern"><![CDATA[(?x) # free form
                 abc # a comment
                 # a line with only a comment
                 123 # another comment It has to see this to fail the match resulting in zero length
              ]]></dfdl:property>
                </dfdl:element>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abcdef]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <data/>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testRegex")
  }

  @Test def testRegexWithFreeFormAndComments2() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <!-- This assert passes only if free form works, and comments work. -->
                <dfdl:assert testKind='pattern'><![CDATA[(?x) # free form
                 abc # a comment
                 # a line with only a comment
                 123 # another comment
              ]]></dfdl:assert>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abc123]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <data>abc123</data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testRegex")
  }

  @Test def testRegexWithFreeFormAndComments3() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <!-- This assert passes only if free form works, and comments work. -->
                <dfdl:assert testKind='pattern'><![CDATA[(?x) # free form
                 abc # a comment
                 # a line with only a comment
                 123 # another comment
              ]]></dfdl:assert>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abcdef]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>assertion failed</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testRegex")
  }
}
