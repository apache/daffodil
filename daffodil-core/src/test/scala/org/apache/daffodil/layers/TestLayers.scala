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

package org.apache.daffodil.layers

import org.apache.daffodil.util._
import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.util.TestUtils
import java.io.ByteArrayOutputStream
import org.apache.commons.io.IOUtils
import java.nio.charset.StandardCharsets
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import scala.collection.JavaConverters._
import org.apache.daffodil.xml.XMLUtils

class TestLayers {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  val B64Layer1Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdlx:layerTransform="base64_MIME" dfdlx:layerLengthKind="boundaryMark" dfdlx:layerBoundaryMark="!" dfdlx:layerEncoding="iso-8859-1">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")

  @Test def testParseB64Layer1(): Unit = {
    val sch = B64Layer1Schema
    val data = "cGxl!" // encoding of "ple" + "!"
    val infoset = <ex:e1 xmlns:ex={ example }><s1>ple</s1></ex:e1>
    val (_, actual) = TestUtils.testString(sch, data)
    TestUtils.assertEqualsXMLElements(infoset, actual)

    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, data, areTracing)
  }

  val B64Layer2Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind='delimited'/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdlx:layerTransform="base64_MIME" dfdlx:layerLengthKind="boundaryMark" dfdlx:layerBoundaryMark="!" dfdlx:layerEncoding="iso-8859-1">
            <xs:element name="s1" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")

  @Test def testParseB64Layer2(): Unit = {
    val sch = B64Layer2Schema
    val data = "cGxl!" // encoding of "ple" + "!"
    val infoset = <ex:e1 xmlns:ex={ example }><s1>ple</s1></ex:e1>
    val (_, actual) = TestUtils.testString(sch, data)
    TestUtils.assertEqualsXMLElements(infoset, actual)

    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, data, areTracing)
  }

  val B64Layer3Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind='delimited'/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdlx:layerTransform="base64_MIME" dfdlx:layerLengthKind="boundaryMark" dfdlx:layerBoundaryMark="!" dfdlx:layerEncoding="iso-8859-1">
              <xs:element name="s1" type="xs:string"/>
            </xs:sequence>
            <xs:element name="s2" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")

  @Test def testParseB64Layer3(): Unit = {
    val sch = B64Layer3Schema
    val data = "cGxl" + "!" + "moreDataAfter"
    val infoset = <ex:e1 xmlns:ex={ example }><s1>ple</s1><s2>moreDataAfter</s2></ex:e1>
    val (_, actual) = TestUtils.testString(sch, data)
    TestUtils.assertEqualsXMLElements(infoset, actual)

    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, data, areTracing)
  }

  def makeGZIPData(text: String) = {
    val baos = new ByteArrayOutputStream()
    val gzos = new java.util.zip.GZIPOutputStream(baos)
    IOUtils.write(text, gzos, StandardCharsets.UTF_8)
    gzos.close()
    val data = baos.toByteArray()
    // Java 16+ sets the 9th byte to 0xFF, but previous Java versions set the
    // value to 0x00. Daffodil always unparses with 0xFF regardless of Java
    // version, so force the gzip data to 0xFF to make sure tests round trip
    data(9) = 0xFF.toByte
    data
  }

  val text = """This is just some made up text that is intended to be
a few lines long. If this had been real text, it would not have been quite
so boring to read. Use of famous quotes or song lyrics or anything like that
introduces copyright notice issues, so it is easier to simply make up
a few lines of pointless text like this.""".replace("\r\n", "\n").replace("\n", " ")

  @Test def testGZIPRoundTrips(): Unit = {
    val bais = new ByteArrayInputStream(makeGZIPData(text))
    val gzis = new java.util.zip.GZIPInputStream(bais)
    val rdr = new InputStreamReader(gzis, StandardCharsets.UTF_8)
    val lines = IOUtils.readLines(rdr)
    val textBack = lines.asScala.head
    assertEquals(text, textBack)
  }

  val GZIPLayer1Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" dfdlx:layerLengthUnits="bytes" representation="binary"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="len" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4" dfdl:outputValueCalc="{ dfdl:contentLength(../x1, 'bytes') }"/>
            <xs:element name="x1" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdlx:layerTransform="gzip" dfdlx:layerLengthKind="explicit" dfdlx:layerLength="{ ../len }">
                  <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified")

  def makeGZIPLayer1Data() = {
    val gzipData = makeGZIPData(text)
    val dataLength = gzipData.length
    val baos = new ByteArrayOutputStream()
    val dos = new java.io.DataOutputStream(baos)
    dos.writeInt(dataLength)
    dos.write(gzipData)
    dos.write("afterGzip".getBytes(StandardCharsets.UTF_8))
    dos.close()
    val data = baos.toByteArray()
    (data, dataLength)
  }

  @Test def testGZIPLayer1(): Unit = {
    val sch = GZIPLayer1Schema
    val (data, dataLength) = makeGZIPLayer1Data()
    val infoset = <ex:e1 xmlns:ex={ example }><len>{ dataLength }</len><x1><s1>{ text }</s1></x1><s2>afterGzip</s2></ex:e1>
    val (_, actual) = TestUtils.testBinary(sch, data, areTracing = false)
    TestUtils.assertEqualsXMLElements(infoset, actual)

    TestUtils.testUnparsingBinary(sch, infoset, data)
  }

  def makeB64GZIPSchema(term: String, layerTerm: String) = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" dfdlx:layerLengthUnits="bytes" representation="binary" dfdlx:layerEncoding="iso-8859-1"/>,
    <xs:element name="e1" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator={ term }/>
          <xs:sequence dfdlx:layerTransform="base64_MIME" dfdlx:layerLengthKind="boundaryMark" dfdlx:layerBoundaryMark={ layerTerm }>
            <xs:sequence>
              <xs:element name="len" type="xs:int" dfdl:outputValueCalc="{ dfdl:contentLength(../x1, 'bytes') }"/>
              <xs:element name="x1" dfdl:lengthKind="implicit">
                <xs:complexType>
                  <xs:sequence dfdlx:layerTransform="gzip" dfdlx:layerLengthKind="explicit" dfdlx:layerLength="{ ../len }">
                    <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
          <xs:element name="s3" type="xs:string" dfdl:lengthKind="delimited"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified")

  def toB64(bytes: Array[Byte]) =
    java.util.Base64.getMimeEncoder.encodeToString(bytes)

  def makeB64GZIPData(term: String, layerTerm: String, before: String, after: String, text: String) = {
    val gzipData = makeGZIPData(text)
    val dataLength = gzipData.length
    val baos = new ByteArrayOutputStream()
    val dos = new java.io.DataOutputStream(baos)
    dos.writeInt(dataLength) // 4 byte length of gzipped data
    dos.write(gzipData)
    dos.close()
    val gzBytes = baos.toByteArray()
    val b64Text = toB64(gzBytes) // encoded as base6
    val baos2 = new ByteArrayOutputStream()
    val dos2 = new java.io.DataOutputStream(baos2)

    dos2.write(before.getBytes(StandardCharsets.UTF_8))
    dos2.write(term.getBytes(StandardCharsets.UTF_8))
    dos2.write(b64Text.getBytes("ascii")) // b64 text is always ascii.
    dos2.write(layerTerm.getBytes("ascii"))
    dos2.write(after.getBytes(StandardCharsets.UTF_8))
    dos2.close()
    (baos2.toByteArray(), dataLength)
  }

  @Test def testParseB64GZIPLayer1(): Unit = {
    val term = ";"
    val layerTerm = "=_END_="
    val sch = makeB64GZIPSchema(term, layerTerm)
    val before = "beforeB64GZip"
    val after = "afterB64GZip"
    val (data, dataLength) = makeB64GZIPData(term, layerTerm, before, after, text)
    val (_, actual) = TestUtils.testBinary(sch, data, areTracing = false)
    val infoset = <ex:e1 xmlns:ex={ example }><s1>{ before }</s1><len>{ dataLength }</len><x1><s2>{ text }</s2></x1><s3>{ after }</s3></ex:e1>
    TestUtils.assertEqualsXMLElements(infoset, actual)

    TestUtils.testUnparsingBinary(sch, infoset, data)
  }

  val lineFoldLayer1Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdlx:layerTransform="lineFolded_IMF" dfdlx:layerLengthKind="implicit">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "qualified")

  /**
   * Has lines folded using IMF conventions.
   *
   * Notice use of the s"""...""" string interpolation. This interprets
   * the escape sequences even though triple quote doesn't.
   */
  val ipsumLorem1 = s"""Lorem ipsum\r\n dolor sit amet"""

  val ipsumLorem1Unfolded = s"""Lorem ipsum dolor sit amet"""

  @Test def testParseLineFoldIMF1(): Unit = {
    val sch = lineFoldLayer1Schema
    val data = ipsumLorem1
    val infoset = <e1 xmlns={ example }><s1>{ ipsumLorem1Unfolded }</s1></e1>
    val (_, actual) = TestUtils.testString(sch, data)
    TestUtils.assertEqualsXMLElements(infoset, actual)
  }

  val ipsumLorem2 = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod\r\n tempor incididunt ut labore et dolore magna aliqua. Ut enim ad"""
  ///////////////////// 123456789012345678901234567890123456789012345678901234567890123456789012 3 4567890123456789012345678901234567890123456789012345678901234567890
  /////////////////////          1         2         3         4         5         6         7           8
  val ipsumLorem2Unfolded = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad"""

  @Test def testUnparseLineFoldIMF1(): Unit = {
    val sch = lineFoldLayer1Schema
    val data = ipsumLorem2
    val infoset = <e1 xmlns={ example }><s1>{ ipsumLorem2Unfolded }</s1></e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, data, areTracing)
  }

  val lineFoldLayer2Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdlx:layerTransform="lineFolded_IMF" dfdlx:layerLengthKind="boundaryMark">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "qualified")

  /**
   * Has lines folded using IMF conventions.
   *
   * Notice use of the s"""...""" string interpolation. This interprets
   * the escape sequences even though triple quote doesn't.
   */
  val ipsumLorem3 = s"""Lorem ipsum\r\n dolor sit amet,\r\nconsectetur adipiscing elit"""

  val ipsumLorem3Unfolded = s"""Lorem ipsum dolor sit amet,"""

  @Test def testParseLineFoldIMF2(): Unit = {
    val sch = lineFoldLayer2Schema
    val data = ipsumLorem3
    val infoset = <e1 xmlns={ example }><s1>{ ipsumLorem3Unfolded }</s1></e1>
    val (_, actual) = TestUtils.testString(sch, data)
    TestUtils.assertEqualsXMLElements(infoset, actual)
  }

  val ipsumLorem4 = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod\r\n tempor incididunt\r\n"""
  ///////////////////// 123456789012345678901234567890123456789012345678901234567890123456789012 3 4567890123456789012345678901234567890123456789012345678901234567890
  /////////////////////          1         2         3         4         5         6         7           8
  val ipsumLorem4Unfolded = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt"""

  @Test def testUnparseLineFoldIMF2(): Unit = {
    val sch = lineFoldLayer2Schema
    val data = ipsumLorem4
    val infoset = <e1 xmlns={ example }><s1>{ ipsumLorem4Unfolded }</s1></e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, data, areTracing)
  }

  /**
   * The length of the layer is constrained by surrounding explicit-length
   * element.
   */
  val lineFoldLayer3Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit" dfdl:length="100">
        <xs:complexType>
          <xs:sequence dfdlx:layerTransform="lineFolded_IMF" dfdlx:layerLengthKind="implicit">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "qualified")

  val ipsumLorem5 = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod\r\n tempor incididunt ut labore et dolore magna aliqua."""
  ///////////////////// 123456789012345678901234567890123456789012345678901234567890123456789012 3 4567890123456789012345678901234567890123456789012345678901234567890
  /////////////////////          1         2         3         4         5         6         7           8         9         A

  val ipsumLorem5Unfolded = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labor"""

  @Test def testParseLineFoldIMF3(): Unit = {
    val sch = lineFoldLayer3Schema
    val data = ipsumLorem5
    val infoset = <e1 xmlns={ example }><s1>{ ipsumLorem5Unfolded }</s1></e1>
    val (_, actual) = TestUtils.testString(sch, data)
    TestUtils.assertEqualsXMLElements(infoset, actual)
  }

  val ipsumLorem6 = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod\r\n tempor incididunt ut labor"""
  ///////////////////// 123456789012345678901234567890123456789012345678901234567890123456789012 3 4567890123456789012345678901234567890123456789012345678901234567890
  /////////////////////          1         2         3         4         5         6         7           8         9         A

  val ipsumLorem6Unfolded = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labor"""

  @Test def testUnparseLineFoldIMF3(): Unit = {
    val sch = lineFoldLayer3Schema
    val data = ipsumLorem6
    val infoset = <e1 xmlns={ example }><s1>{ ipsumLorem6Unfolded }</s1></e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, data, areTracing)
  }

  val le32BitData = Array[Byte](0x01,                   // BE MSBF
                                0x43, 0x33, 0x33, 0x32, // fourbyteswap + LE LSBF (parsed right to left four bytes at a time)
                                            0x55, 0x54, // fourbyteswap + LE LSBF
                                0x67)                   // BE MSBF

  val le32BitSchema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" bitOrder="leastSignificantBitFirst"
                   byteOrder="littleEndian" alignmentUnits="bits" alignment="1"
                   lengthKind="explicit" lengthUnits="bits"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s0" type="xs:hexBinary" dfdl:length="4" dfdl:byteOrder="bigEndian" dfdl:bitOrder="mostSignificantBitFirst"/>
            <xs:element name="s1" type="xs:hexBinary" dfdl:length="4" dfdl:byteOrder="bigEndian" dfdl:bitOrder="mostSignificantBitFirst"/>
            <xs:sequence dfdlx:layerTransform="fourbyteswap" dfdlx:layerLengthKind="explicit" dfdlx:layerLengthUnits="bytes" dfdlx:layerLength="6">
              <xs:sequence>
                <xs:element name="s2" type="xs:hexBinary" dfdl:length="4"/>
                <xs:element name="s3" type="xs:hexBinary" dfdl:length="24"/>
                <xs:element name="s4" type="xs:hexBinary" dfdl:length="8"/>
                <xs:element name="s5" type="xs:hexBinary" dfdl:length="12"/>
              </xs:sequence>
            </xs:sequence>
            <xs:element name="s6" type="xs:hexBinary" dfdl:length="4" dfdl:byteOrder="bigEndian" dfdl:bitOrder="mostSignificantBitFirst"/>
            <xs:element name="s7" type="xs:hexBinary" dfdl:length="4" dfdl:byteOrder="bigEndian" dfdl:bitOrder="mostSignificantBitFirst"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "qualified")

  @Test def testFourByteSwapLayer(): Unit = {
    val sch = le32BitSchema
    val data = le32BitData
    val infoset =
      <e1 xmlns={ example }>
        <s0>00</s0>
        <s1>10</s1>
        <s2>02</s2>
        <s3>333333</s3>
        <s4>44</s4>
        <s5>5505</s5>
        <s6>60</s6>
        <s7>70</s7>
      </e1>

    val (_, actual) = TestUtils.testBinary(sch, data, areTracing = false)
    TestUtils.assertEqualsXMLElements(infoset, actual)

    TestUtils.testUnparsingBinary(sch, infoset, data)
  }
}
