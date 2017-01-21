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

package edu.illinois.ncsa.daffodil.outputValueCalc

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util.TestUtils

/*
 * These are all tests of OVC expressions that forward reference
 * (and backward. It's a mixture)
 */
class TestOutputValueCalcForwardReference {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testOutputValueCalcForwardReference1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "22", areTracing)
  }

  @Test def testOutputValueCalcForwardReference2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" textTrimKind="padChar" textStringPadCharacter="%SP;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s3 }"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s3>3</s3></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "333", areTracing)
  }

  @Test def testOutputValueCalcForwardReference3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "222", areTracing)
  }

  @Test def testOutputValueCalcDeadlock() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s3 }"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }></ex:e1>
    val areTracing = false
    
    try {
      TestUtils.testUnparsing(sch, infoset, "222", areTracing)
      fail("Expected SuspendedExpressionsDeadlockException")
    } catch {
      case e: Exception => {
        val msg = e.getMessage().toLowerCase()
        assertTrue(msg.contains("deadlocked"))
        assertTrue(msg.contains("runtime schema definition error"))
      }
    }

  }

  @Test def testOutputValueCalcConstant() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:bar xmlns:ex={ example }><afterFoo>fghij</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "abcdefghij", areTracing)
  }

  @Test def testOutputValueCalcAfterOptional() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",%#x20;">
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo=" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:initiator="foo=" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:initiator="afterFoo=" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:bar xmlns:ex={ example }><beforeFoo>12345</beforeFoo><afterFoo>67890</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "beforeFoo=12345, foo=abcde, afterFoo=67890", areTracing)
  }

  @Test def testMultipleOutputValueCalcAndDefaultablePresent() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",%#x20;">
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo=" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" dfdl:initiator="foo=" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="foo2" dfdl:initiator="foo2=" type="xs:string" default="fghij" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
            <xs:element name="afterFoo" dfdl:initiator="afterFoo=" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:bar xmlns:ex={ example }><beforeFoo>12345</beforeFoo><foo2>pqrst</foo2><afterFoo>67890</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "beforeFoo=12345, foo=abcde, foo2=pqrst!, afterFoo=67890", areTracing)
  }

}
