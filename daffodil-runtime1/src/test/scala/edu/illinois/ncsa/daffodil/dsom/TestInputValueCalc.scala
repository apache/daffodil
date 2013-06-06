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


import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.compiler._
import org.junit.Test
import scala.xml.Node
import org.junit.Test

class TestInputValueCalc extends Logging {

  // @Test
  @Test def testInputValueCalc1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:textNumberRep="standard" dfdl:representation="text" dfdl:terminator="" dfdl:emptyValueDelimiterPolicy="none" dfdl:inputValueCalc="{ 42 }" dfdl:initiator="" dfdl:lengthKind="explicit" dfdl:length="1"/>)
    val actual = TestUtils.testString(testSchema, "")
    val actualString = actual.result.toString
    val expected: Node = <data>42</data>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  // @Test
  @Test def testInputValueCalcString2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val actual = TestUtils.testString(testSchema, "A")
    val actualString = actual.result.toString
     val expected: Node = <data><e1>A</e1><e2>A</e2></data>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  // @Test
  @Test def testInputValueCalcInt3() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="e2" type="xs:int" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val actual = TestUtils.testString(testSchema, "8")
    val actualString = actual.result.toString
    val expected: Node = <data><e1>8</e1><e2>8</e2></data>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }
}
