/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.dsom

import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util._
import org.apache.daffodil.Implicits._
import org.junit.Test

class TestDsomCompiler2 extends Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource
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
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    //    val actual = TestUtils.testString(testSchema, "*word")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">word</data>"))

    val infoset = <data xmlns={ example }>word</data>
    TestUtils.testUnparsing(testSchema, infoset, "*word")
  }

  @Test def testTerminator() = {
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:terminator="!" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    //    val actual = TestUtils.testString(testSchema, "37!")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={ example }>37</data>
    TestUtils.testUnparsing(testSchema, infoset, "37!")
  }

  @Test def testDelims() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:terminator="! $" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    //    val actual = TestUtils.testString(testSchema, "*37$")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={ example }>37</data>
    TestUtils.testUnparsing(testSchema, infoset, "*37!")
  }

  @Test def testUnparseMultiElem1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //    val actual = TestUtils.testString(testSchema, "943.2801")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>943.28</somedata><moredata>1</moredata></list>"))

    val infoset = <list xmlns={ example }><somedata>943.28</somedata><moredata>1</moredata></list>
    // TODO: unparse needs to restore leading zeros removed in parse?
    //testUnparsing(testSchema, infoset, "943.2801")
    TestUtils.testUnparsing(testSchema, infoset, "943.281")
  }

  @Test def testUnparseMultiElem2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //    val actual = TestUtils.testString(testSchema, "50.93^%XYZ^42")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>"))

    val infoset = <list xmlns={ example }><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>
    TestUtils.testUnparsing(testSchema, infoset, "50.93^%XYZ^42")
  }

  @Test def testUnparseNested() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //    val actual = TestUtils.testString(testSchema, "11235.8qwerty")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>"))

    val infoset = <list xmlns={ example }><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>
    TestUtils.testUnparsing(testSchema, infoset, "11235.8qwerty")
  }

  @Test def testUnparseNestedChildren() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //    val actual = TestUtils.testString(testSchema, "abc87654321")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>"))

    val infoset = <list xmlns={ example }><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>
    TestUtils.testUnparsing(testSchema, infoset, "abc87654321")
  }

  @Test def testUnparseDelimited() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //        val actual = TestUtils.testString(testSchema, "246813579,90.3761,hello,100")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>"))

    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>
    TestUtils.testUnparsing(testSchema, infoset, "246813579,90.3761,hello,100")
  }

  @Test def testUnparseAlignmentBits() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    TestUtils.testUnparsing(testSchema, infoset, "246813579,90.3761,abc,10034567")
  }

  @Test def testUnparseChoice1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //        val actual = TestUtils.testString(testSchema, "567,word,choice1:203867")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><c>203867</c></choice></list>"))

    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><c>203867</c></choice></list>
    TestUtils.testUnparsing(testSchema, infoset, "567,word,choice1:203867")
  }

  @Test def testUnparseChoice2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    //        val actual = TestUtils.testString(testSchema, "567,word,choice2:2038.67")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>"))

    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>
    TestUtils.testUnparsing(testSchema, infoset, "567,word,choice2:2038.67")
  }

  @Test def testUnparseBinaryIntBE() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary"/>)
    //        val actual = TestUtils.testBinary(testSchema, "0000000F")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<data"))
    //        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={ example }>15</data>
    val bytes = List[Byte](0, 0, 0, 15).toArray
    TestUtils.testUnparsingBinary(testSchema, infoset, bytes)
  }

  @Test def testUnparseBinaryIntLE() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary" dfdl:byteOrder='littleEndian'/>)
    //        val actual = TestUtils.testBinary(testSchema, "0F000000")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<data"))
    //        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={ example }>15</data>
    val bytes = List[Byte](15, 0, 0, 0).toArray
    TestUtils.testUnparsingBinary(testSchema, infoset, bytes)
  }

  @Test def testUnparseBinary1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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
    val bytes = List[Byte](31, -1, -112, 0, 0, 0, 0, 0, 0, 4, 6).toArray
    TestUtils.testUnparsingBinary(testSchema, infoset, bytes)
  }
}
