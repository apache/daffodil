/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import org.junit.Test
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._

class TestDsomCompilerUnparse1 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testUnparse1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "12", areTracing)
  }

  /**
   * Test emphasis on delimiter unparsers
   */
  @Test def testUnparse2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:initiator="[" dfdl:separator="," dfdl:terminator="]">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "[1,2]")
  }

  /**
   * Test emphasis on StringDelimitedUnparser
   */
  @Test def testUnparse3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:initiator="[" dfdl:separator="," dfdl:terminator="]">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "[1,2]")
  }

  /**
   * Test emphasis on StringDelimitedUnparser w/ padding
   */
  @Test def testUnparse4() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;" truncateSpecifiedLengthString="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:initiator="!#" dfdl:terminator="#!">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="left"/>
            <xs:element name="s2" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="right"/>
            <xs:element name="s3" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="center"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:simpleType name="st">
        <xs:restriction base="xs:string">
          <xs:maxLength value="3"/>
          <xs:minLength value="3"/>
        </xs:restriction>
      </xs:simpleType>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2><s3>3</s3></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "!#1__,__2,_3_#!")
  }

  /**
   * Test emphasis on StringDelimitedUnparser w/ escape scheme
   */
  @Test def testUnparse5() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>
      <dfdl:defineEscapeScheme name="pound">
        <dfdl:escapeScheme escapeCharacter='#' escapeKind="escapeCharacter" escapeEscapeCharacter="" extraEscapedCharacters="" generateEscapeBlock="whenNeeded"/>
      </dfdl:defineEscapeScheme>
      <dfdl:defineEscapeScheme name='cStyleComment'>
        <dfdl:escapeScheme escapeBlockStart='/*' escapeBlockEnd='*/' escapeKind="escapeBlock" escapeEscapeCharacter="#" generateEscapeBlock="whenNeeded" extraEscapedCharacters=""/>
      </dfdl:defineEscapeScheme>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" dfdl:escapeSchemeRef="pound"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:escapeSchemeRef="cStyleComment"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>one, two</s1><s2>, three and four*/</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "one#, two,/*, three and four#*/*/", areTracing)
  }

}
