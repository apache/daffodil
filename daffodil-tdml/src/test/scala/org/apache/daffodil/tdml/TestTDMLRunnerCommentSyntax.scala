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

package edu.illinois.ncsa.daffodil.tdml

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class TestTDMLRunnerCommentSyntax {
  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  @Test def testCommentsInScalaLiteralXML() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <!-- comment -->
        <tdml:defineSchema name="mySchema">
          <!-- comment -->
          <dfdl:format ref="tns:daffodilTest1" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <!-- comment -->
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <!-- comment -->
          <xsd:complexType name="arrayType">
            <!-- comment -->
            <xsd:sequence dfdl:separator="|">
              <!-- comment -->
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
              <!-- comment -->
            </xsd:sequence>
            <!-- comment -->
          </xsd:complexType>
          <!-- comment -->
        </tdml:defineSchema>
        <!-- comment -->
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="array" model="mySchema">
          <!-- comment -->
          <tdml:document>
            <!-- comment -->
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
            <!-- comment -->
          </tdml:document>
          <!-- comment -->
          <tdml:infoset>
            <!-- comment -->
            <tdml:dfdlInfoset>
              <!-- comment -->
              <array>
                <!-- comment -->
                <data>1</data>
                <!-- comment -->
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
                <!-- comment -->
              </array>
              <!-- comment -->
            </tdml:dfdlInfoset>
            <!-- comment -->
          </tdml:infoset>
          <!-- comment -->
        </tdml:parserTestCase>
        <!-- comment -->
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("test1")
    // if we don't throw/fail when running then pass this test.
  }

}
