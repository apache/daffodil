package edu.illinois.ncsa.daffodil.api

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


import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestDFDLParser extends JUnitSuite {

  @Test def testParseSimple1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = Compiler.testString(sch, "5678").result
    val expected: Node = <e1>5678</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseSequenceOfJustOneScalar() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5").result
    val actualString = actual.toString
    val expected = <e1><s1>5</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseSequence1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "56").result
    val actualString = actual.toString
    val expected = <e1><s1>5</s1><s2>6</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseSequence2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "567").result
    val actualString = actual.toString
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseSequence3() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5,6,7").result
    val actualString = actual.toString
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  /* This test is in daffodil-test.
  @Test def testParseSequence4() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:initiator="[" dfdl:terminator="]">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:initiator="{" dfdl:terminator="}">
            <xs:element name="s1" type="xs:string" dfdl:initiator="(" dfdl:terminator=")" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[{(5),(6),(7)}]").result
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s1>6</s1><s1>7</s1></e1>"))
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }*/

  /**
   * Shows that delimiters longer than 1 character work.
   * Shows that simple expressions like 1 + 1 get evaluated to constants
   * Shows that initiator and terminator work on a sequence as well as on elements.
   */
  /* This test is in daffodil-test.
  @Test def testParseSequence5() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:initiator="[more[" dfdl:terminator="]nomore]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:string" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[more[{{((55)),,((66)),,((77))}}]nomore]").result
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>55</s1><s1>66</s1><s1>77</s1></e1>"))
    val expected = <e1><s1>55</s1><s1>66</s1><s1>77</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }*/

  @Test def testInt1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "55,000").result
    TestUtils.assertEqualsXMLElements(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  @Test def testInt2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val e = intercept[Exception] {
      val actual = Compiler.testString(sch, "55.001")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:int"))
  }

  @Test def testShort1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "55,000").result
    TestUtils.assertEqualsXMLElements(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  @Test def testShort2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val e = intercept[Exception] {
      val actual = Compiler.testString(sch, "70,000")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:short"))
  }

  @Test def testByte1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
            <xs:element name="s2" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "55123").result
    TestUtils.assertEqualsXMLElements(<e1><s1>55</s1><s2>123</s2></e1>, actual)
  }

  @Test def testNumber1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="country" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="area" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="region" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="number" type="xs:long" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "1-800-555-1212").result
    TestUtils.assertEqualsXMLElements(<e1><country>1</country><area>-800</area><region>-555</region><number>-1212</number></e1>, actual)
  }

  @Test def testNumber2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="mersenne" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = Compiler.testString(sch, "-127").result
    TestUtils.assertEqualsXMLElements(<mersenne>-127</mersenne>, actual)
  }

  @Test def testNumber3() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="perfect" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val actual = Compiler.testString(sch, "+3").result
    TestUtils.assertEqualsXMLElements(<perfect>3</perfect>, actual)
  }

  @Test def testUnsignedLong1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="needs-square-root" type="xs:unsignedLong" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val e = intercept[Exception] {
      val actual = Compiler.testString(sch, "-3")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedLong"))
  }

  @Test def testUnsignedInt1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="world-population" type="xs:unsignedInt" dfdl:lengthKind="explicit" dfdl:length="{ 13 }"/>)
    val e = intercept[Exception] {
      // As of 18:31 UTC (EST+5) Jun 8, 2012
      val actual = Compiler.testString(sch, "7,018,631,476")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedInt"))
  }

  @Test def testUnsignedShort1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="last-element-indicator" type="xs:unsignedShort" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val e = intercept[Exception] {
      val actual = Compiler.testString(sch, "-1")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedShort"))
  }

  @Test def testUnsignedByte1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bits" type="xs:unsignedByte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>)
    val e = intercept[Exception] {
      val actual = Compiler.testString(sch, "256")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedByte"))
  }

  // TEST FAILS - SEE JIRA DFDL-184
  //  @Test def testIntTooLong() {
  //    val sch = TestUtils.dfdlTestSchema(
  //      <dfdl:format ref="tns:daffodilTest1"/>,
  //      <xs:element name="e1" dfdl:lengthKind="explicit">
  //        <xs:complexType>
  //          <xs:sequence>
  //            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 20 }"/>
  //          </xs:sequence>
  //        </xs:complexType>
  //      </xs:element>)
  //       val e = intercept[Exception]{
  //      val actual = Compiler.testString(sch, "55555555555555555555")
  //    }
  //    //println("ERROR!!!!!" + e.getMessage())//    assertTrue(e.getMessage().contains("xs:int"))
  //  }

  @Test def testParseSequenceInt() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:initiator="[[" dfdl:terminator="]]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:int" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[[{{((55)),,((66)),,((77))}}]]").result
    val actualString = actual.toString
    val expected = <e1><s1>55</s1><s1>66</s1><s1>77</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBadInt() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>)
    val e = intercept[Exception] {
      val actual = Compiler.testString(sch, "A").result
    }
    //println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:int"))
  }

  @Test def testParseOccursCountKindOfParsed() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5678A").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedWithTerminator() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;A").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedByTerminator() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator="."/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;A.").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedByTerminator2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;.").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedBySeparator() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator=";">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;.").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryIntMinusOne() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "FFFFFFFF").result
    val expected = <e1><s1>-1</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryInts() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="implicit" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:int" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "000000013bFFFFFFFF3b080402013b000000003bFFFFFF7F").result
    val expected = <e1><s1>1</s1><s1>-1</s1><s1>134480385</s1><s1>0</s1><s2>2147483647</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryDoubleOne() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:double" dfdl:lengthKind="implicit"/>)
    val actual = Compiler.testBinary(sch, "3FF0000000000000").result
    val expected = <e1>1.0</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryDoubleMinusOne() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:double" dfdl:lengthKind="implicit"/>)
    val actual = Compiler.testBinary(sch, "BFF0000000000000").result
    val expected = <e1>-1.0</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryDoubleLSB() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:double" dfdl:lengthKind="implicit"/>)
    val actual = Compiler.testBinary(sch, "0000000000000001").result
    val expected = <e1>4.9E-324</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryDoubles() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:double" dfdl:lengthKind="implicit" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:double" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "3FF00000000000003bBFF00000000000003b08040201080402013b00000000000000003bFFFFFFFFFFFFFF7F").result
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.7340609871421765E-270</s1><s1>0.0</s1><s2>NaN</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testTextDoubles() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 8 }"/>
            <xs:element name="s3" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "01.0;-1.0;4.15;0.31;8.6E-2001,234.9").result
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>8.6E-200</s2><s3>1234.9</s3></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testBinaryFloats() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:float" dfdl:lengthKind="implicit" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:float" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "3F8000003bBF8000003b080402013b000000003b0000C07F").result
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>3.972466E-34</s1><s1>0.0</s1><s2>NaN</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testTextFloats() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
            <xs:element name="s3" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "01.0;-1.0;4.15;0.31;-7.1E81,234.9").result
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>-7.1E8</s2><s3>1234.9</s3></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

}

