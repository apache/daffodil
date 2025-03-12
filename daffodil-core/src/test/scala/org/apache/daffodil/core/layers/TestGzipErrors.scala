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

package org.apache.daffodil.core.layers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.xml.Elem

import org.apache.daffodil.core.util.FuzzOneBits
import org.apache.daffodil.core.util.FuzzRandomByteRuns
import org.apache.daffodil.core.util.FuzzRandomSingleByte
import org.apache.daffodil.core.util.FuzzZeroBits
import org.apache.daffodil.core.util.LayerParseTester
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.apache.commons.io.IOUtils
import org.junit.Test

class TestGzipErrors {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  val text =
    """This is just some made up text that is intended to be
a few lines long. If this had been real text, it would not have been quite
so boring to read. Use of famous quotes or song lyrics or anything like that
introduces copyright notice issues, so it is easier to simply make up
a few lines of pointless text like this.""".replace("\r\n", "\n").replace("\n", " ")

  val GZIPLayer1Schema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xs:import namespace="urn:org.apache.daffodil.layers.gzip"
                     schemaLocation="/org/apache/daffodil/layers/xsd/gzipLayer.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="binary"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:choice>
            <xs:element name="e1" dfdl:lengthKind="implicit"
                        xmlns:gz="urn:org.apache.daffodil.layers.gzip">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="len" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
                              dfdl:outputValueCalc="{ dfdl:contentLength(../x1/data, 'bytes') }"/>
                  <xs:element name="x1" dfdl:lengthKind="explicit" dfdl:length="{ ../len }">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element name="data">
                          <xs:complexType>
                            <xs:sequence dfdlx:layer="gz:gzip">
                              <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                  <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="hex" type="xs:hexBinary" dfdl:lengthKind="delimited" dfdl:encoding="iso-8859-1"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

  def makeGZIPData(text: String) = {
    val baos = new ByteArrayOutputStream()
    val gzos = new java.util.zip.GZIPOutputStream(baos)
    IOUtils.write(text, gzos, StandardCharsets.UTF_8)
    gzos.close()
    val data = baos.toByteArray()
    // Java 16+ sets the 9th byte to 0xFF, but previous Java versions set the
    // value to 0x00. Daffodil always unparses with 0xFF regardless of Java
    // version, so force the gzip data to 0xFF to make sure tests round trip
    data(9) = 0xff.toByte
    data
  }

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

  @Test def testGZIPLayerOk1(): Unit = {
    val sch = GZIPLayer1Schema
    val (data, _) = makeGZIPLayer1Data()
    val infoset =
      <ex:root xmlns:ex={example}>
      <e1>
        <len>{212}</len>
        <x1>
        <data>
          <s1>{text}</s1>
        </data>
      </x1>
        <s2>afterGzip</s2>
      </e1>
    </ex:root>
    val (_, actual) = TestUtils.testBinary(sch, data, areTracing = false)
    XMLUtils.compareAndReport(infoset, actual)
  }

  @Test def testGZIPLayerErr1(): Unit = {
    val sch = GZIPLayer1Schema
    val (data, _) = makeGZIPLayer1Data()
    // clobber half the data
    ((data.length / 2) to data.length - 1).foreach { i => data(i) = 0 }
    // This causes the gzip input stream to throw an IOException
    val infoset =
      <ex:root xmlns:ex={example}>
        <hex><![CDATA[000000D41F8B08000000000000FF4D904176C3200C44AF3207C8F33DBA6F0F40CCD85683918B44D3DC3EC2C9A2EFB1013EF3357C6E6288F5DDCD61BA137BCA443FE0FC73F8967C5C4B75D6CC0C575C8984857714A93414ADEB848F25D800B794036045632A67C605E2B86B2F19553D800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000]]></hex>
      </ex:root>
    val (_, actual) = TestUtils.testBinary(sch, data, areTracing = false)
    XMLUtils.compareAndReport(infoset, actual)
  }

  @Test def testGZIPLayerErr2(): Unit = {
    val sch = GZIPLayer1Schema
    val (data, _) = makeGZIPLayer1Data()
    // clobber last 10% of the data with 0xFF bytes.
    ((data.length - (data.length / 10)) to data.length - 1).foreach { i => data(i) = -1 }
    // This causes the gzip input stream to throw an IOException
    val infoset =
      <ex:root xmlns:ex={example}>
        <hex><![CDATA[000000D41F8B08000000000000FF4D904176C3200C44AF3207C8F33DBA6F0F40CCD85683918B44D3DC3EC2C9A2EFB1013EF3357C6E6288F5DDCD61BA137BCA443FE0FC73F8967C5C4B75D6CC0C575C8984857714A93414ADEB848F25D800B794036045632A67C605E2B86B2F19553D805FBE889F2ECE70E2AA4DEA3AA2E3519EF065842E58D2AEDD02530F8DB640832A8F26F3B94DF511CA712437BE27ADDE34F739F8598F20D7CD875566460BEBB4CB10CAD989C9846D684DF6A33CA2F9ED6CFEBF5DCC7168C4169ABDBEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]]></hex>
      </ex:root>
    val (_, actual) = TestUtils.testBinary(sch, data, areTracing = false)
    XMLUtils.compareAndReport(infoset, actual)
  }

  val GZIPLayerErrSchema =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xs:import namespace="urn:org.apache.daffodil.layers.gzip"
                     schemaLocation="/org/apache/daffodil/layers/xsd/gzipLayer.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="binary"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" dfdl:lengthKind="implicit"
                        xmlns:gz="urn:org.apache.daffodil.layers.gzip">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="len" type="xs:unsignedInt" dfdl:lengthKind="explicit" dfdl:length="4"
                              dfdl:outputValueCalc="{ dfdl:contentLength(../x1/data, 'bytes') }"/>
                  <xs:element name="x1" dfdl:lengthKind="explicit" dfdl:length="{ ../len }">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element name="data">
                          <xs:complexType>
                            <xs:sequence dfdlx:layer="gz:gzip">
                              <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                  <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

  /**
   * These are parse errors due to IOExceptions that occur.
   */
  val excludes: Seq[String] = Seq(
    "EOFException",
    "Corrupt GZIP trailer",
    "Unexpected end of ZLIB",
    "invalid distance too far back",
    "Not in GZIP format",
    "invalid literal/lengths set",
    "invalid bit length repeat",
    "invalid code lengths set",
    "invalid code -- missing end-of-block",
    "invalid distances set",
    "Unsupported compression method",
    "Insufficient bits in data",
    "too many length or distance symbols",
    "invalid stored block lengths",
    "invalid block type",
    "Corrupt GZIP header",
    "invalid distance code",
    "invalid literal/length code"
  )

  val expected = <root xmlns="http://example.com">
    <e1>
      <len>212</len> <x1>
      <data>
        <s1>This is just some made up text that is intended to be a few lines long. If this had been real text, it would not have been quite so boring to read. Use of famous quotes or song lyrics or anything like that introduces copyright notice issues, so it is easier to simply make up a few lines of pointless text like this.</s1>
      </data>
    </x1> <s2>afterGzip</s2>
    </e1>
  </root>

  // fuzz0 - writes runs of 0 bytes of random length in random locations
  def fuzz0(nTrials: Int, sch: Elem, data: Array[Byte], expected: Elem): Unit = {
    var shouldFail = false
    val fuzzer = new FuzzZeroBits(data, 4, 0)
    val tester = new LayerParseTester(sch, data, expected, fuzzer, excludes)
    tester.run(nTrials)
  }

  // fuzz1 - writes runs of 0xFF (all 1 bits) bytes of random length in random locations
  def fuzz1(nTrials: Int, sch: Elem, data: Array[Byte], expected: Elem): Unit = {
    var shouldFail = false
    val fuzzer = new FuzzOneBits(data, 4, 0)
    val tester = new LayerParseTester(sch, data, expected, fuzzer, excludes)
    tester.run(nTrials)
  }

  // fuzz2 only modifies 1 byte at a time in the data with a random (but always different) byte
  def fuzz2(nTrials: Int, sch: Elem, data: Array[Byte], expected: Elem): Unit = {
    val fuzzer =
      new FuzzRandomSingleByte(data, 4, 9) // leave first 4 bytes and last 9 bytes alone.
    val tester = new LayerParseTester(sch, data, expected, fuzzer, excludes)
    tester.run(nTrials)
  }

  // fuzz3 modifies runs of bytes of random length.
  def fuzz3(nTrials: Int, sch: Elem, data: Array[Byte], expected: Elem): Unit = {
    var shouldFail = false
    val fuzzer = new FuzzRandomByteRuns(data, 4, 0)
    val tester = new LayerParseTester(sch, data, expected, fuzzer, excludes)
    tester.run(nTrials)
  }

  // Can be run for 100,000 trials - 11 cases fuzzed data did not stop the parse
  @Test def testGZIPLayerFuzz0(): Unit = {
    fuzz0(10, sch = GZIPLayerErrSchema, data = makeGZIPLayer1Data()._1, expected)
  }

  // Can be run for 100,000 trials - 11 cases fuzzed data did not stop the parse
  @Test def testGZIPLayerFuzz1(): Unit = {
    fuzz1(10, sch = GZIPLayerErrSchema, data = makeGZIPLayer1Data()._1, expected)
  }

  // Can be run for 100,000 trials
  @Test def testGZIPLayerFuzz2(): Unit = {
    fuzz2(10, sch = GZIPLayerErrSchema, data = makeGZIPLayer1Data()._1, expected)
  }

  // This has been run for 100K trials
  // which only takes a few seconds, and
  // all errors thrown were converted to parse errors
  // so that means they were all IOExceptions from the gzip layer.
  @Test def testGZIPLayerFuzz3(): Unit = {
    // 2000 trials gives us good test coverage of the fuzz testing framework.
    fuzz3(2000, sch = GZIPLayerErrSchema, data = makeGZIPLayer1Data()._1, expected)
  }

}
