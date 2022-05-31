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
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse
import org.apache.daffodil.util._
import org.junit.Test
import org.apache.daffodil.Implicits._
import org.junit.Assert.fail

class UnitTestTDMLRunner {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  @Test def testDocPart1(): Unit = {
    val xml = <documentPart type="text">abcde</documentPart>
    val dp = new TextDocumentPart(xml, null)
    val actual = Misc.bits2Bytes(dp.dataBits).toList
    val expected = Vector('a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte, 'e'.toByte).toList
    assertEquals(expected, actual)
  }

  @Test def testDocPart2(): Unit = {
    val xml = <document><documentPart type="byte">123abc</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("123abc", hexDigits)
    val actual = doc.documentBytes.toList
    val expected = Vector(0x12, 0x3a, 0xbc).map { _.toByte }.toList
    assertEquals(expected, actual)
  }

  @Test def testHexOddNumberOfNibbles(): Unit = {
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

  @Test def testDocPart3(): Unit = {
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

  @Test def testDocWithMultiByteUnicode(): Unit = {
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

  @Test def testDocWithDFDLEntities(): Unit = {
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

  @Test def testDocWithTextFile(): Unit = {
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
    val runner = new Runner(xml)
    val ts = runner.getTS
    val ptc = ts.parserTestCases(0)
    val doc = ptc.document.get
    doc.documentParts(0)
    val bytes = new Array[Byte](20)
    val numBytes = doc.data.read(bytes)
    val actual = new String(bytes, 0, numBytes, "UTF8").replace("\r\n", "\n")
    assertEquals("test\n1\n2\n3\n", actual)
    runner.reset
  }

  @Test def testDocWithBinaryFile(): Unit = {
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
    val runner = new Runner(xml)
    val ts = runner.getTS
    val ptc = ts.parserTestCases(0)
    val doc = ptc.document.get
    doc.documentParts(0)
    val bytes = new Array[Byte](4)
    doc.data.read(bytes)
    val actual = bytes.toList
    val expected = Vector(0xDE, 0xAD, 0xBE, 0xEF).map { _.toByte }.toList
    assertEquals(expected, actual)
    runner.reset
  }

  @Test def test1(): Unit = {
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

    val runner = new Runner(xml)
    val ts = runner.getTS
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
    val trimmed = XMLUtils.removeAttributes(actualContent)
    val expected = <byte1>123</byte1>
    assertEquals(expected, trimmed)
    runner.reset
  }

  @Test def test2(): Unit = {
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

    val runner = new Runner(xml)
    val ts = runner.getTS
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
    val trimmed = XMLUtils.removeAttributes(actualContent)
    val expected = <byte1>123</byte1>
    assertEquals(expected, trimmed)
    runner.reset
  }

  @Test def testTDMLResource(): Unit = {
    val res = Misc.getRequiredResource("/test-suite/ibm-contributed/dpaext1-2.tdml")
    val runner = new Runner(res)
    val ts = runner.getTS
    val mf = ts.findTDMLResource("./fvt/ext/dpa/dpaspc121_01.dfdl.xsd")
    val file = new File(mf.get)
    assertTrue(file.exists())
    runner.reset
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

  @Test def testEmbeddedSchemaValidates(): Unit = {
    val testSuite = tdmlWithEmbeddedSchemaInvalid
    val runner = new Runner(testSuite)
    val ts = runner.getTS
    assertFalse(ts.isTDMLFileValid)
    val msgs: String = ts.loadingDiagnosticMessages
    val hasMsg = msgs.contains("notAllowed")
    assertTrue(hasMsg)
    assertFalse(ts.isTDMLFileValid)
    runner.reset
  }

  @Test def testTDMLSelfContainedFileValidates(): Unit = {
    val tmpTDMLFile = File.createTempFile("daffodil-tdml-", ".dfdl.xsd")
    val testSuite = tdmlWithEmbeddedSchemaInvalid
    try {
      using(new java.io.FileWriter(tmpTDMLFile)) {
        fw =>
          fw.write(testSuite.toString())
      }

      val runner = new Runner(tmpTDMLFile)
      val ts = runner.getTS
      assertFalse(ts.isTDMLFileValid)
      val msgs = ts.loadingDiagnosticMessages
      assertTrue(msgs.contains("notAllowed"))
      runner.reset

    } finally {
      tmpTDMLFile.delete()
    }
  }

  @Test def testBits(): Unit = {
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
  @Test def testDocWithDoubleNewlines(): Unit = {
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

  @Test def testLSB1(): Unit = {
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

  @Test def testLSB2(): Unit = {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">010</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: BitsDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x02).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_utf8_1char(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst">1</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: TextDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x31).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_7bit_1char(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-US-ASCII-7-BIT-PACKED">1</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: TextDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x31).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_7bit_2char(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-US-ASCII-7-BIT-PACKED">12</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: TextDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x31, 0x19).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB4(): Unit = {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">00110011 110</documentPart>
              </document>

    val doc = new Document(xml, null)
    doc.documentParts.collect { case x: BitsDocumentPart => x }
    val actual = doc.documentBytes.toList
    val expected = List(0x33, 0x6).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testCheckForBadBitOrderTransitions1(): Unit = {
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

  @Test def testCheckForBadBitOrderTransitions2(): Unit = {
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

  @Test def testCheckForBadBitOrderTransitions3(): Unit = {
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

  @Test def testCheckForBadBitOrderTransitions4(): Unit = {
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

  @Test def testCheckForBadBitOrderTransitions5(): Unit = {
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

  val tdmlDefineSchemaWithoutDefaultNamespace =
    <testSuite xmlns:tns={ tns }
                    xmlns:tdml={ tdml }
                    xmlns={ tdml }
                    xmlns:dfdl={ dfdl }
                    xmlns:xsd={ xsd }
                    xmlns:xs={ xsd }
                    xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema" useDefaultNamespace="false" xmlns={ xsd }>
        <dfdl:format ref="tns:GeneralFormat"/>
        <element name="data" type="int" dfdl:lengthKind="delimited"/>
      </tdml:defineSchema>
      <parserTestCase name="test1" root="data" model="mySchema">
        <document>37</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ example }>37</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </testSuite>

  @Test def testDefineSchemaWithNoDefaultNamespace(): Unit = {
    val testSuite = tdmlDefineSchemaWithoutDefaultNamespace
    val runner = new Runner(testSuite)
    val ts = runner.getTS
    if (!ts.isTDMLFileValid) {
      val msgs = ts.loadingExceptions.map{ _.getMessage() } .mkString("\n")
      fail(msgs)
    }
    val tc = ts.parserTestCases.find(ptc => ptc.tcName == "test1")
    val ds = ts.embeddedSchemas.find(ds => ds.name == "mySchema").get
    val dataElem = ds.globalElementDecls.find(edecl => (edecl \ "@name").text == "data").get
    assertTrue(dataElem ne null)
    runner.reset
  }

  @Test def testCommentBit(): Unit = {
    val xml = <document bitOrder="LSBFirst"><documentPart type="bits">00000010 //this is a label111</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("00000010", firstPart.bitDigits)
  }

  @Test def testCommentBitWithNewLine(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">01 01 11 //flagByte1
                                        1 //bool2</documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("0101111", firstPart.bitDigits)
  }

    @Test def testCommentBitJustComments(): Unit = {
      val xml = <document bitOrder="LSBFirst">
                  <documentPart type="bits">
                  // this doc part contains no bits
                  // at all. It is just comments.
                  // 101010101
                  </documentPart>
                </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("", firstPart.bitDigits)
  }

  @Test def testCommentBitNoLineEnding(): Unit = {
    val xml = <document bitOrder="LSBFirst"><documentPart type="bits">01011010 // just a comment here no line ending </documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("01011010", firstPart.bitDigits)
  }

  @Test def testCommentBitBothCommentFormatsNewLine(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">0100110110 /*C0mment 01011111 _01*/11
100111//D1fferent sty1e c0mment</documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("010011011011100111", firstPart.bitDigits)
  }

  @Test def testCommentBitBothCommentFormats(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">0100110110 /*C0mment 01011111 _01*/100111//D1fferent sty1e c0mment</documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("0100110110100111", firstPart.bitDigits)
  }

  @Test def testBitBadCommentFormatException(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">0100110110 C0mment 01011111 _01100*/111//D1fferent sty1e c0mment</documentPart>
              </document>
    val exc = intercept[TDMLException] {
      val doc = new Document(xml, null)
      doc.documentParts.collect { case x: BitsDocumentPart => x }
      val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
      val firstPart = dp(0).bitDigits
    }
    assertTrue(exc.getMessage().contains("Improper formatting of /* */ style comment"))
  }

  @Test def testCommentBitNoWarningCharacters(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">01|01|00
                                        (10).[01]</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("0101001001", firstPart.bitDigits)
  }

  @Test def testCommentBitNoWarningCharactersWithInvalid(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">01|01|00 !!
                                        (10).[01]</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("0101001001", firstPart.bitDigits)
  }

  @Test def testCommentBitNonGreedy(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">0101 /*Data 1*/ 0101 /*Data 2*/</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("01010101", firstPart.bitDigits)
  }

  @Test def testCommentBitNonGreedyNewLine(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">0101 /*Data 1
                                              Explanation*/
                                        0101 /*Data 2
                                              Explanation*/
              </documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("01010101", firstPart.bitDigits)
  }

  @Test def testCommentBitCarriageReturn(): Unit = {
    val xml = <document bitOrder="LSBFirst">
              <documentPart type="bits">0101&#13;00 /*Data 1*/
                                        0101 /*Data 2*/
              </documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    assertEquals("0101000101", firstPart.bitDigits)
  }

  @Test def testCommentByte(): Unit = {
    val xml = <document><documentPart type="byte">12 3A BC.abc //Label (ABCDEF123456789</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("123ABCabc", hexDigits)
  }

    @Test def testCommentByteWithNewLine(): Unit = {
    val xml = <document><documentPart type="byte">123ABCabc //Label (ABCDEF123456789
      456DEFdef //New Label</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("123ABCabc456DEFdef", hexDigits)
  }

  @Test def testCommentByteBothCommentFormatsNewLine(): Unit = {
    val xml = <document><documentPart type="byte">12AB3C /*Comment ABC123 ** */
    45D6d//Different style comment</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("12AB3C45D6d", hexDigits)
  }

  @Test def testCommentByteBothCommentFormats(): Unit = {
    val xml = <document><documentPart type="byte">12AB3C /*Comment ABC123 ** */45D6d//Different style comment</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("12AB3C45D6d", hexDigits)
  }

  @Test def testByteBadCommentFormatException(): Unit = {
    val xml = <document><documentPart type="byte">12AB3C Comment ABC123 ** */45D6d//Different style comment</documentPart></document>
    val exc = intercept[TDMLException] {
      val doc = new Document(xml, null)
      val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
      val hexDigits = dp(0).hexDigits
    }
    assertTrue(exc.getMessage().contains("Improper formatting of /* */ style comment"))
  }

  @Test def testCommentByteNoWarningCharacters(): Unit = {
    val xml = <document><documentPart type="byte">01|01|00
                                                  (AB).[AB]</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("010100ABAB", hexDigits)
  }

  @Test def testCommentByteNoWarningCharactersWithInvalid(): Unit = {
    val xml = <document><documentPart type="byte">01|01|00 !!
                                                  (AB).[AB]</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("010100ABAB", hexDigits)
  }

  @Test def testCommentByteCommentNonGreedy(): Unit = {
    val xml = <document><documentPart type="byte">0101AB /*Data 1*/ 0101ab /*Data 2*/</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("0101AB0101ab", hexDigits)
  }

  @Test def testCommentByteNonGreedyNewLine(): Unit = {
    val xml = <document>
                <documentPart type="byte">
                  <![CDATA[
                    0101AB /*Data 1
                            Explanation
                            `0123456789
                            [;,]'.\/'
                            */
                    0101ab /*Data 2
                            Explanation
                            ~)!@#$%^&*(
                            {:<}">|?
                            */
                  ]]>
                </documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("0101AB0101ab", hexDigits)
  }

  @Test def testCommentByteCarriageReturn(): Unit = {
    val xml = <document>
                <documentPart type="byte">
                    01&#13;01AB /*Data 1*/
                    0101ab /*Data 2*/
                </documentPart>
              </document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: ByteDocumentPart => x }
    val hexDigits = dp(0).hexDigits
    assertEquals("0101AB0101ab", hexDigits)
  }

  @Test def testMIL2045_47001D_Page70_TableB_I_With_Comment_Syntax_1(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" byteOrder="RTL">/*Version*/                         XXXX 0011</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*FPI*/                             XXX0 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*Compression*/                     NA       </documentPart>
        <documentPart type="bits" byteOrder="RTL">/*GPI for Originator Address*/      XX1X XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*FPI for URN*/                     X1XX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*URN*/  X0000000 00000000 01100111 1XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*FPI for Unit Name*/               1XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*Unit Name*/                       X101 0101</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    0XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    XX10 0111</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    01XX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    XXX1 0010</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    100X XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    XXXX 1010</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    0001 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    XXXX X100</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    1111 1XXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                    XXXX XX11</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*GPI for Recip. Addr Group*/       XXXX X1XX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*GRI for R_ONE*/                   XXXX 0XXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*FPI for URN*/                     XXX1 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">/*URN*/ XXXX00000 00000000 00000000 011X XXXX</documentPart>
      </document>, null)
    val doc1bits = doc.documentBits
    doc1bits.length
    val doc2 = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte"><![CDATA[
            E3 67 00 80 55 67 92 1A FC 77 00 00 00
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits
    assertEquals(doc2bits, doc1bits)
  }

  @Test def testMIL2045_47001D_Page70_TableB_I_With_Comment_Syntax_2(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" byteOrder="RTL">XXXX 0011                          //Version </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXX0 XXXX                              //FPI </documentPart>
        <documentPart type="bits" byteOrder="RTL">NA                             //Compression </documentPart>
        <documentPart type="bits" byteOrder="RTL">XX1X XXXX       //GPI for Originator Address </documentPart>
        <documentPart type="bits" byteOrder="RTL">X1XX XXXX                      //FPI for URN </documentPart>
        <documentPart type="bits" byteOrder="RTL">X0000000 00000000 01100111 1XXX XXXX   //URN </documentPart>
        <documentPart type="bits" byteOrder="RTL">1XXX XXXX                //FPI for Unit Name </documentPart>
        <documentPart type="bits" byteOrder="RTL">X101 0101                        //Unit Name </documentPart>
        <documentPart type="bits" byteOrder="RTL">0XXX XXXX                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">XX10 0111                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">01XX XXXX                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXX1 0010                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">100X XXXX                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXXX 1010                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">0001 XXXX                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXXX X100                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">1111 1XXX                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXXX XX11                                    </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXXX X1XX        //GPI for Recip. Addr Group </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXXX 0XXX                    //GRI for R_ONE </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXX1 XXXX                      //FPI for URN </documentPart>
        <documentPart type="bits" byteOrder="RTL">XXXX00000 00000000 00000000 011X XXXX  //URN </documentPart>
      </document>, null)
    val doc1bits = doc.documentBits
    doc1bits.length
    val doc2 = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte"><![CDATA[
            E3 67 00 80 55 67 92 1A FC 77 00 00 00
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits
    assertEquals(doc2bits, doc1bits)
  }

}