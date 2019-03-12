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

package org.apache.daffodil.tdml

import java.io.File
import org.apache.daffodil.Implicits.using
import org.apache.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.assertFalse
import org.apache.daffodil.util._
import org.junit.Test
import org.apache.daffodil.Implicits._

class UnitTestTDMLRunner {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  @Test def testDocPart1() {
    val xml = <documentPart type="text">abcde</documentPart>
    val dp = new TextDocumentPart(xml, null)
    val actual = Misc.bits2Bytes(dp.dataBits).toList
    val expected = Vector('a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte, 'e'.toByte).toList
    assertEquals(expected, actual)
  }

  @Test def testDocPart2() {
    val xml = <document><documentPart type="byte">123abc</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("123abc", hexDigits)
    val actual = doc.documentBytes.toList
    val expected = Vector(0x12, 0x3a, 0xbc).map { _.toByte }.toList
    assertEquals(expected, actual)
  }

  @Test def testHexOddNumberOfNibbles() {
    val xml = <document><documentPart type="byte">123</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("123", hexDigits)
    val actual = doc.documentBytes.toList
    val expected = Array(0x12, 0x30).map { _.toByte }.toList
    assertEquals(expected, actual)
    assertEquals(12, doc.nBits)
  }

  @Test def testDocPart3() {
    val xml = <document>
                <documentPart type="byte">12</documentPart>
                <documentPart type="byte">3abc</documentPart>
              </document>

    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val firstPart = dp(0)
    val secondPart = dp(1)
    assertEquals("12", firstPart.hexDigits)
    assertEquals("3abc", secondPart.hexDigits)
    val actual = doc.documentBytes.toList
    val expected = Vector(0x12, 0x3a, 0xbc).map { _.toByte }.toList
    assertEquals(expected, actual)
  }

  @Test def testDocWithMultiByteUnicode() {
    val xml = <document>
                <documentPart type="text">&#x24;&#xA2;&#x20AC;&#x2028;</documentPart>
              </document>
    val doc = new Document(xml, null)
    val docPart = doc.documentParts(0)
    val bytes = doc.documentBytes
    assertEquals(9, bytes.length)
    val str = docPart.partRawContent // a string
    assertEquals(4, str.length)
    assertEquals("$¢€\u2028", str)
  }

  @Test def testDocWithDFDLEntities() {
    val xml = <document>
                <documentPart replaceDFDLEntities="true" type="text">\%#x24;%#xA2;%#x20AC;%%%NUL;%LS;%%#IGNORED;</documentPart>
              </document>
    val doc = new Document(xml, null)
    doc.documentParts(0)
    val bytes = doc.documentBytes
    assertEquals(24, bytes.length)
    val orig = new String(bytes.toArray, "UTF8")
    assertEquals(19, orig.length)
    assertEquals("\\$¢€%%\u0000\u2028%%#IGNORED;", orig)
  }

  @Test def testDocWithTextFile() {
    val xml = <testSuite xmlns={ tdml } ID="suite identifier" suiteName="theSuiteName" description="Some Test Suite Description">
                <parserTestCase name="test1" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd" description="Some test case description.">
                  <document>
                    <documentPart type="file">test/tdml/test.txt</documentPart>
                  </document>
                  <infoset>
                    <dfdlInfoset xmlns:xs={ xsd } xmlns:xsi={ xsi }>
                      <byte1 xsi:type="xs:byte">123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>
    val ts = new DFDLTestSuite(xml)
    val ptc = ts.parserTestCases(0)
    val doc = ptc.document.get
    doc.documentParts(0)
    val bytes = new Array[Byte](20)
    val numBytes = doc.data.read(bytes)
    val actual = new String(bytes, 0, numBytes, "UTF8").replace("\r\n", "\n")
    assertEquals("test\n1\n2\n3\n", actual)
  }

  @Test def testDocWithBinaryFile() {
    val xml = <testSuite xmlns={ tdml } ID="suite identifier" suiteName="theSuiteName" description="Some Test Suite Description">
                <parserTestCase name="test1" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd" description="Some test case description.">
                  <document>
                    <documentPart type="file">test/tdml/test.bin</documentPart>
                  </document>
                  <infoset>
                    <dfdlInfoset xmlns:xs={ xsd } xmlns:xsi={ xsi }>
                      <byte1 xsi:type="xs:byte">123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>
    val ts = new DFDLTestSuite(xml)
    val ptc = ts.parserTestCases(0)
    val doc = ptc.document.get
    doc.documentParts(0)
    val bytes = new Array[Byte](4)
    doc.data.read(bytes)
    val actual = bytes.toList
    val expected = Vector(0xDE, 0xAD, 0xBE, 0xEF).map { _.toByte }.toList
    assertEquals(expected, actual)
  }

  @Test def test1() {
    val xml = <testSuite xmlns={ tdml } ID="suite identifier" suiteName="theSuiteName" description="Some Test Suite Description">
                <parserTestCase name="test1" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd" description="Some test case description.">
                  <document>0123</document>
                  <infoset>
                    <dfdlInfoset xmlns:xs={ xsd } xmlns:xsi={ xsi }>
                      <byte1>123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>

    val ts = new DFDLTestSuite(xml)
    val ptc = ts.parserTestCases(0)
    assertEquals("test1", ptc.tcName)
    assertEquals("byte1", ptc.rootName)
    assertEquals("test-suite/ibm-contributed/dpanum.dfdl.xsd", ptc.model)
    assertTrue(ptc.description.contains("Some test case description."))
    val doc = ptc.document.get
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte).toList
    val actualBytes = doc.documentBytes.toList
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.optExpectedInfoset.get
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1>123</byte1>
    assertEquals(expected, trimmed)
  }

  @Test def test2() {
    val xml = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                <parserTestCase ID="some identifier" name="test2" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd">
                  <document>0123</document>
                  <infoset>
                    <dfdlInfoset xmlns:xs={ xsd } xmlns:xsi={ xsi }>
                      <byte1>123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>

    lazy val ts = new DFDLTestSuite(xml)
    val tsn = ts.suiteName
    assertEquals("theSuiteName", tsn)
    assertEquals("", ts.description)
    val ptc = ts.parserTestCases(0)
    assertEquals("", ptc.description)
    assertEquals("test2", ptc.tcName)
    assertEquals("byte1", ptc.rootName)
    assertEquals("test-suite/ibm-contributed/dpanum.dfdl.xsd", ptc.model)
    val doc = ptc.document.get
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte).toList
    val actualBytes = doc.documentBytes.toList
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.optExpectedInfoset.get
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1>123</byte1>
    assertEquals(expected, trimmed)
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  @Test def testTDMLResource() {
    lazy val res = Misc.getRequiredResource("/test-suite/ibm-contributed/dpaext1-2.tdml")
    lazy val ts = new DFDLTestSuite(new File(res))
    val mf = ts.findTDMLResource("./fvt/ext/dpa/dpaspc121_01.dfdl.xsd")
    val file = new File(mf.get)
    assertTrue(file.exists())
  }
  val tdmlWithEmbeddedSchemaInvalid =
    <tdml:testSuite suiteName="testEmbeddedSchemaValidates" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:GeneralFormat"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="notAllowed" dfdl:notAProp="{ 2 }"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testEmbeddedSchemaValidates" root="data" model="mySchema">
        <document>37</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ example }>37</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testEmbeddedSchemaValidates() {
    val testSuite = tdmlWithEmbeddedSchemaInvalid
    assertFalse(ts.isTDMLFileValid)
    lazy val ts = new DFDLTestSuite(testSuite)
    assertFalse(ts.isTDMLFileValid)
    val msgs: String = ts.loadingDiagnosticMessages
    val hasMsg = msgs.contains("notAllowed")
    // println("messages = '" + msgs + "'")
    assertTrue(hasMsg)
    assertFalse(ts.isTDMLFileValid)
  }
  @Test def testTDMLSelfContainedFileValidates() {
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = tdmlWithEmbeddedSchemaInvalid
    try {
      using(new java.io.FileWriter(tmpTDMLFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }

      lazy val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
      assertFalse(ts.isTDMLFileValid)
      val msgs = ts.loadingDiagnosticMessages
      assertTrue(msgs.contains("notAllowed"))

    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
  }

  @Test def testBits() {
    val doc = new Document(<document><documentPart type="bits">111</documentPart></document>, null)
    // val bits = doc.documentParts(0)
    val bytes = doc.documentBytes.toList
    assertEquals(-32, bytes(0))
    assertEquals(3, doc.nBits)
  }
  /**
   * Make sure our tdml data document preserves CRLFs.
   * <p>
   * Note that you can't put them into the tdml file as CRLF actual characters because
   * XML loading of the tdml file doesn't preserve them. The TDML runner never
   * sees the CR in that case because the XML loader being used to load the
   * tdml file has removed the CRLF and replaced it with just LF.
   */
  @Test def testDocWithDoubleNewlines() {
    val xml = <tdml:document>
                <tdml:documentPart type="text">ab</tdml:documentPart>
                <tdml:documentPart type="byte">0d0a0d0a</tdml:documentPart>
                <tdml:documentPart type="text">cd</tdml:documentPart>
              </tdml:document>
    val d = new Document(xml, null)
    val actual = d.documentBytes
    val expected = Vector(
      'a'.toByte, 'b'.toByte,
      0x0d.toByte, 0x0a.toByte,
      0x0d.toByte, 0x0a.toByte,
      'c'.toByte, 'd'.toByte).toList
    assertEquals(expected, actual.toList)
  }

  @Test def testLSB1() {
    val xml = <document bitOrder="LSBFirst">
                <documentPart type="bits">00000010</documentPart>
              </document>

    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("00000010", firstPart.bitDigits)
    val actual = doc.documentBytes.toList
    val expected = List(0x02).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB2() {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">010</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: BitsDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x02).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_utf8_1char() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst">1</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: TextDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x31).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_7bit_1char() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-US-ASCII-7-BIT-PACKED">1</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: TextDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x31).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_7bit_2char() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-US-ASCII-7-BIT-PACKED">12</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: TextDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x31, 0x19).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB4() {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">00110011 110</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: BitsDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x33, 0x6).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testCheckForBadBitOrderTransitions1() {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">1</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">1</documentPart>
              </document>
    val exc = intercept[Exception] {
      val doc = new Document(xml, null)
      doc.documentParts.collect { case x: BitsDocumentPart => x }
    }
    assertTrue(exc.getMessage().contains("bitOrder"))
  }

  @Test def testCheckForBadBitOrderTransitions2() {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">1</documentPart>
                <documentPart type="bits" bitOrder="LSBFirst">111 1111</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">1</documentPart>
              </document>
    val doc = new Document(xml, null)
    // No error in this case because the change of bit order is on a byte boundary.
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    assertEquals(3, dp.length)
  }

  @Test def testCheckForBadBitOrderTransitions3() {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">1111 1111</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">111 1111</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">1</documentPart>
              </document>
    val doc = new Document(xml, null)
    // No error in this case because the change of bit order is on a byte boundary.
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    assertEquals(3, dp.length)
  }

  @Test def testCheckForBadBitOrderTransitions4() {
    val xml = <document>
                <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst">abcdefgh</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">111 1111</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">1</documentPart>
              </document>
    val doc = new Document(xml, null)
    // No error in this case because the change of bit order is on a byte boundary.
    val dp = doc.documentParts.collect { case x: DocumentPart => x }
    assertEquals(3, dp.length)
  }

  @Test def testCheckForBadBitOrderTransitions5() {
    val xml = <document>
                <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst">abc</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">111 1111</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">1</documentPart>
              </document>
    val exc = intercept[Exception] {
      val doc = new Document(xml, null)
      doc.documentParts.collect { case x: DocumentPart => x }
    }
    assertTrue(exc.getMessage().contains("bitOrder"))
  }
}
