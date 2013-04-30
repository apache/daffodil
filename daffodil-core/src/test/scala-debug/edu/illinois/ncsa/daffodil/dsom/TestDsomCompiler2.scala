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


import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.util.Misc
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.io.FileOutputStream
import java.nio.channels.WritableByteChannel
import java.io.FileWriter
import java.io.File
import java.nio.ByteBuffer
import org.junit.Test

class TestDsomCompiler2 extends Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

  @Test def testInitiator() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    //    val actual = Compiler.testString(testSchema, "*word")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">word</data>"))

    val infoset = <data xmlns={ example }>word</data>
    Compiler.testUnparsing(testSchema, infoset, "*word")
  }

  @Test def testTerminator() = {
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:terminator="!" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    //    val actual = Compiler.testString(testSchema, "37!")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={ example }>37</data>
    Compiler.testUnparsing(testSchema, infoset, "37!")
  }

  @Test def testDelims() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:terminator="! $" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    //    val actual = Compiler.testString(testSchema, "*37$")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={ example }>37</data>
    Compiler.testUnparsing(testSchema, infoset, "*37!")
  }

  @Test def testUnparseMultiElem1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="somedata" type="xs:float" dfdl:length="6" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:int" dfdl:length="2" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "943.2801")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>943.28</somedata><moredata>1</moredata></list>"))

    val infoset = <list xmlns={ example }><somedata>943.28</somedata><moredata>1</moredata></list>
    // TODO: unparse needs to restore leading zeros removed in parse?
    //testUnparsing(testSchema, infoset, "943.2801")
    Compiler.testUnparsing(testSchema, infoset, "943.281")
  }

  @Test def testUnparseMultiElem2() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="^">
          <xs:element name="somedata" type="xs:double" dfdl:length="5" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit" dfdl:initiator="%"/>
          <xs:element name="anddata" type="xs:int" dfdl:length="2" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "50.93^%XYZ^42")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>"))

    val infoset = <list xmlns={ example }><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>
    Compiler.testUnparsing(testSchema, infoset, "50.93^%XYZ^42")
  }

  @Test def testUnparseNested() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="somedata" type="tns:example2">
            <xs:annotation>
              <xs:appinfo source={ dfdl }>
                <dfdl:element alignmentUnits="bytes"/>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
      <xs:complexType name="example2">
        <xs:sequence>
          <xs:element name="moredata" type="xs:double" dfdl:length="7" dfdl:lengthKind="explicit"/>
          <xs:element name="anddata" type="xs:string" dfdl:length="6" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "11235.8qwerty")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>"))

    val infoset = <list xmlns={ example }><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>
    Compiler.testUnparsing(testSchema, infoset, "11235.8qwerty")
  }

  @Test def testUnparseNestedChildren() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="data" type="tns:example2">
            <xs:annotation>
              <xs:appinfo source={ dfdl }>
                <dfdl:element alignmentUnits="bytes"/>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
      <xs:complexType name="example2">
        <xs:sequence>
          <xs:element name="somedata" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:int" dfdl:length="8" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "abc87654321")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>"))

    val infoset = <list xmlns={ example }><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>
    Compiler.testUnparsing(testSchema, infoset, "abc87654321")
  }

  @Test def testUnparseDelimited() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="delimited"/>
          <xs:element name="b" type="xs:double" dfdl:lengthKind="delimited"/>
          <xs:element name="c" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="d" type="xs:int" dfdl:lengthKind="delimited"/>
        </xs:sequence>
      </xs:complexType>)
    //        val actual = Compiler.testString(testSchema, "246813579,90.3761,hello,100")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>"))

    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>
    Compiler.testUnparsing(testSchema, infoset, "246813579,90.3761,hello,100")
  }

  @Test def testUnparseAlignmentBits() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bits"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="delimited"/>
          <xs:element name="b" type="xs:double" dfdl:lengthKind="delimited"/>
          <xs:element name="c" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit"/>
          <xs:element name="d" type="xs:int" dfdl:length="8" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>abc</c><d>10034567</d></list>
    Compiler.testUnparsing(testSchema, infoset, "246813579,90.3761,abc,10034567")
  }

  @Test def testUnparseChoice1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bits"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
          <xs:element name="choice">
            <xs:complexType>
              <xs:choice dfdl:choiceLengthKind="implicit">
                <xs:element name="c" type="xs:int" dfdl:initiator="choice1:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
                <xs:element name="d" type="xs:double" dfdl:initiator="choice2:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)
    //        val actual = Compiler.testString(testSchema, "567,word,choice1:203867")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><c>203867</c></choice></list>"))

    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><c>203867</c></choice></list>
    Compiler.testUnparsing(testSchema, infoset, "567,word,choice1:203867")
  }

  @Test def testUnparseChoice2() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bits"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
          <xs:element name="choice">
            <xs:complexType>
              <xs:choice dfdl:choiceLengthKind="implicit">
                <xs:element name="c" type="xs:int" dfdl:initiator="choice1:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
                <xs:element name="d" type="xs:double" dfdl:initiator="choice2:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)
    //        val actual = Compiler.testString(testSchema, "567,word,choice2:2038.67")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>"))

    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>
    Compiler.testUnparsing(testSchema, infoset, "567,word,choice2:2038.67")
  }

  @Test def testUnparseBinaryIntBE() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary"/>)
    //        val actual = Compiler.testBinary(testSchema, "0000000F")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<data"))
    //        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={ example }>15</data>
    val bytes = Array[Byte](0, 0, 0, 15)
    Compiler.testUnparsingBinary(testSchema, infoset, bytes)
  }

  @Test def testUnparseBinaryIntLE() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary" dfdl:byteOrder='littleEndian'/>)
    //        val actual = Compiler.testBinary(testSchema, "0F000000")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<data"))
    //        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={ example }>15</data>
    val bytes = Array[Byte](15, 0, 0, 0)
    Compiler.testUnparsingBinary(testSchema, infoset, bytes)
  }

  //  @Test def testBMP() {
  //    val infoset = scala.xml.XML.loadFile("header.xml")
  //    val compiler = Compiler()
  //    val pf = compiler.compile("BMPtest.xsd") //pass compiler the schema
  //    val unparser = pf.onPath("/") //create unparser for given schema
  //    val outputStream = new FileOutputStream("testBMP")
  //    val out = java.nio.channels.Channels.newChannel(outputStream)
  //    unparser.unparse(out, infoset) //pass infoset to unparser
  //    out.close()
  //  }

  //  @Test def testBMPPixels() {
  //    val infoset = scala.xml.XML.loadFile("pixeldata.xml")
  //    val compiler = Compiler()
  //    val pf = compiler.compile("BMPtestPixels.xsd") //pass compiler the schema
  //    val unparser = pf.onPath("/") //create unparser for given schema
  //    val outputStream = new FileOutputStream("testBMPPixels")
  //    val out = java.nio.channels.Channels.newChannel(outputStream)
  //    unparser.unparse(out, infoset) //pass infoset to unparser
  //    out.close()
  //  }

  @Test def testUnparseBinary1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="byte" type="xs:byte" dfdl:length="2" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
          <xs:element name="short" type="xs:short" dfdl:length="4" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
          <xs:element name="long" type="xs:long" dfdl:length="4" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
        </xs:sequence>
      </xs:complexType>)

    val infoset = <list xmlns={ example }><byte>31</byte><short>-112</short><long>1030</long></list>
    val bytes = Array[Byte](31, -1, -112, 0, 0, 0, 0, 0, 0, 4, 6)
    Compiler.testUnparsingBinary(testSchema, infoset, bytes)
  }
}

