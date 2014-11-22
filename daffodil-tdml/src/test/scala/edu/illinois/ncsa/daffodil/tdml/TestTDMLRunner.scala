package edu.illinois.ncsa.daffodil.tdml

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

import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility
import scala.xml.XML
import edu.illinois.ncsa.daffodil.Implicits.using
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.assertFalse
import junit.framework.Assert.fail
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._

class TestTDMLRunner {

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
    val docPart = doc.documentParts(0)
    val bytes = doc.documentBytes
    assertEquals(22, bytes.length)
    val orig = new String(bytes.toArray, "UTF8")
    assertEquals(17, orig.length)
    assertEquals("\\$¢€%\u0000\u2028%#IGNORED;", orig)
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
    val docPart = doc.documentParts(0)
    val bb = java.nio.ByteBuffer.allocate(11)
    doc.data.read(bb)
    val bytes = bb.array()
    val actual = new String(bytes.toArray, "UTF8")
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
    val docPart = doc.documentParts(0)
    val bb = java.nio.ByteBuffer.allocate(4)
    doc.data.read(bb)
    val bytes = bb.array()
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
                      <byte1 xsi:type="xs:byte">123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>

    val ts = new DFDLTestSuite(xml)
    val ptc = ts.parserTestCases(0)
    assertEquals("test1", ptc.name)
    assertEquals("byte1", ptc.root)
    assertEquals("test-suite/ibm-contributed/dpanum.dfdl.xsd", ptc.model)
    assertTrue(ptc.description.contains("Some test case description."))
    val doc = ptc.document.get
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte).toList
    val actualBytes = doc.documentBytes.toList
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset.get
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
    assertEquals("test2", ptc.name)
    assertEquals("byte1", ptc.root)
    assertEquals("test-suite/ibm-contributed/dpanum.dfdl.xsd", ptc.model)
    val doc = ptc.document.get
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte).toList
    val actualBytes = doc.documentBytes.toList
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset.get
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1>123</byte1>
    assertEquals(expected, trimmed)
  }

  val testSchema = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>)

  @Test def testTDMLParseSuccess() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseSuccess" root="data">
                        <ts:document>37</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLParseSuccess", Some(testSchema))
  }

  @Test def testTDMLParseDetectsErrorWithSpecificMessage() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsErrorWithSpecificMessage" root="data">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error>AA</ts:error><!-- can have several substrings of message -->
                          <ts:error>xs:int</ts:error><!-- all are checked against the message -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLParseDetectsErrorWithSpecificMessage", Some(testSchema))
  }

  @Test def testTDMLParseDetectsErrorWithPartMessage() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsErrorWithPartMessage" root="data">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error>AA</ts:error>
                          <ts:error>xs:float</ts:error><!-- Detect this mismatch. It will say xs:int -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val exc = intercept[Exception] {
      ts.runOneTest("testTDMLParseDetectsErrorWithPartMessage", Some(testSchema))
    }
    assertTrue(exc.getMessage().contains("""message "xs:float""""))
  }

  @Test def testTDMLParseDetectsErrorAnyMessage() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsErrorAnyMessage" root="data">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error/><!-- don't care what message is -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLParseDetectsErrorAnyMessage", Some(testSchema))
  }

  @Test def testTDMLParseDetectsNoError() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsNoError" root="data">
                        <ts:document>37</ts:document>
                        <ts:errors>
                          <ts:error/><!-- don't care what message is -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val exc = intercept[Exception] {
      ts.runOneTest("testTDMLParseDetectsNoError", Some(testSchema))
    }
    // println(exc)
    assertTrue(exc.getMessage().contains("Expected error"))
  }

  // TODO: Implement Warnings
  //
  //  @Test def testTDMLParseDetectsNoWarning() {
  //
  //    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
  //                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsNoWarning" root="data">
  //                        <ts:document>37</ts:document>
  //                        <ts:infoset>
  //                          <ts:dfdlInfoset>
  //                            <data xmlns={ example }>37</data>
  //                          </ts:dfdlInfoset>
  //                        </ts:infoset>
  //                        <ts:warnings>
  //                          <ts:warning/><!-- don't care what message is -->
  //                        </ts:warnings>
  //                      </ts:parserTestCase>
  //                    </ts:testSuite>
  //    lazy val ts = new DFDLTestSuite(testSuite)
  //    val exc = intercept[Exception] {
  //      ts.runOneTest("testTDMLParseDetectsNoWarning", Some(testSchema))
  //    }
  //    assertTrue(exc.getMessage().contains("Did not find"))
  //  }

  @Test def testTDMLParseRunAll() {
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testTDMLParseRunAll1" root="data">
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                      <parserTestCase name="testTDMLParseRunAll2" root="data">
                        <document>92</document>
                        <infoset>
                          <dfdlInfoset xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                            <data>92</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runAllTests(Some(testSchema))
  }

  @Test def testRunModelFile() {
    val tmp = File.createTempFile(getClass.getName(), ".dfdl.xsd")
    tmp.deleteOnExit()
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunModelFile" root="data" model={ tmp.getAbsolutePath() }>
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>

    using(new java.io.FileWriter(tmp)) {
      fileWriter =>
        fileWriter.write(testSchema.toString())
    }
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testRunModelFile")
  }

  @Test def testRunTDMLFileReferencingModelFile() {
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunTDMLFileReferencingModelFile" root="data" model={ tmpFileName }>
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>
    try {
      using(new java.io.FileWriter(tmpFileName)) {
        fw =>
          fw.write(testSchema.toString())
      }
      using(new java.io.FileWriter(tmpTDMLFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }
      lazy val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
      ts.runAllTests()
    } finally {
      try {
        val f = new java.io.File(tmpFileName)
        f.delete()
      } finally {
        val t = new java.io.File(tmpTDMLFileName)
        t.delete()
      }
    }
  }

  @Test def testTDMLResource() {
    lazy val res = Misc.getRequiredResource("/test-suite/ibm-contributed/dpaext1.tdml")
    lazy val ts = new DFDLTestSuite(new File(res))
    val mf = ts.findTDMLResource("./fvt/ext/dpa/dpaspc121_01.dfdl.xsd")
    val file = new File(mf.get)
    assertTrue(file.exists())
  }

  val tdmlWithEmbeddedSchema =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testEmbeddedSchemaWorks" root="data" model="mySchema">
        <document>37</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ example }>37</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testEmbeddedSchemaWorks() {
    val testSuite = tdmlWithEmbeddedSchema
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testEmbeddedSchemaWorks")
  }

  val tdmlWithEmbeddedSchemaInvalid =
    <tdml:testSuite suiteName="testEmbeddedSchemaValidates" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
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
    lazy val ts = new DFDLTestSuite(testSuite)
    assertFalse(ts.isTDMLFileValid)
    val msgs = ts.getLoadingDiagnosticMessages()
    val hasMsg = msgs.contains("notAllowed")
    // println("messages = '" + msgs + "'")
    assertTrue(hasMsg)
  }

  @Test def testRunTDMLSelfContainedFile() {
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = tdmlWithEmbeddedSchema
    try {
      using(new java.io.FileWriter(tmpTDMLFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }
      lazy val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
      ts.runAllTests()
    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
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
      val msgs = ts.getLoadingDiagnosticMessages()
      assertTrue(msgs.contains("notAllowed"))

    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
  }

  val tdmlWithUnicode2028 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:string" dfdl:encoding="utf-8" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testMultiByteUnicodeWorks" root="data" model="mySchema">
        <document>a&#x2028;!</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ tns }>a&#x2028;</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testMultiByteUnicodeWorks() {
    val testSuite = tdmlWithUnicode2028
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testMultiByteUnicodeWorks")
  }

  val tdmlWithUnicode5E74AndCDATA =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:string" dfdl:encoding="utf-8" dfdl:lengthKind="delimited" dfdl:terminator=""/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testMultiByteUnicodeWithCDATAWorks" root="data" model="mySchema">
        <document><documentPart type="text"><![CDATA[a年]]></documentPart></document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ tns }>a&#x5E74;</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testMultiByteUnicodeWithCDATAWorks() {
    val testSuite = tdmlWithUnicode5E74AndCDATA
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testMultiByteUnicodeWithCDATAWorks")
  }

  @Test def testBits() {
    val doc = new Document(<document><documentPart type="bits">111</documentPart></document>, null)
    val bits = doc.documentParts(0)
    val bytes = doc.documentBytes.toList
    assertEquals(-32, bytes(0))
    assertEquals(3, doc.nBits)
  }

  @Test def testNilCompare() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1" nilKind="literalValue" nilValueDelimiterPolicy="terminator"/>
          <xsd:element name="data" type="xsd:int" nillable="true" dfdl:lengthKind="delimited" dfdl:nilValue="nil" dfdl:terminator=";"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>nil;</tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data xsi:nil="true"/>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testNilCompare")
  }

  @Test def testNilCompare2() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <dfdl:format ref="tns:daffodilTest1" nilKind="literalValue" nilValueDelimiterPolicy="terminator"/>
          <xsd:element name="data" type="xsd:int" nillable="true" dfdl:lengthKind="delimited" dfdl:nilValue="nil" dfdl:terminator=";"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>0;</tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data xsi:nil='true'/>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("testNilCompare")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Comparison failed"))
    assertTrue(msg.contains("xsi:nil=\"true\""))
  }

  /**
   * This test shows that we can import any byte at all as an iso-8859-1 character.
   */
  @Test def testAllBytesISO8859() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited" dfdl:encoding="iso-8859-1"/>
          <dfdl:format ref="tns:daffodilTest1"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>
            <!-- leave out the characters for &, ", < and > because they cause us trouble in constructing the expected string result. -->
            <tdml:documentPart type="byte"><![CDATA[
00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f              
10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f 
20 21    23 24 25    27 28 29 2a 2b 2c 2d 2e 2f 
30 31 32 33 34 35 36 37 38 39 3a 3b    3d    3f 
40 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f 
50 51 52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f 
60 61 62 63 64 65 66 67 68 69 6a 6b 6c 6d 6e 6f 
70 71 72 73 74 75 76 77 78 79 7a 7b 7c 7d 7e 7f 
80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 
90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f 
a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af 
b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf 
c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf 
d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df 
e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 ea eb ec ed ee ef 
f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 fa fb fc fd fe ff  

]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <data><![CDATA[   !#$%'()*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ]]></data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val bytes = ts.parserTestCases(0).document.get.documentBytes
    assertEquals(252, bytes.length)
    ts.runOneTest("testNilCompare")
  }

  /**
   * This test is about a corner case when a byte containing 0xFF is encountered.
   * (There was a bug with PagedSeq[Byte] returning -1 for byte 255, making things
   * behave like end-of-data when a byte of 0xFF was encountered.
   */
  @Test def testISO8859_FF_Byte() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited" dfdl:encoding="iso-8859-1"/>
          <dfdl:format ref="tns:daffodilTest1"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="byte"><![CDATA[
31 32 33 ff ff ff 34 35 36
]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data>123ÿÿÿ456</tns:data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val bytes = ts.parserTestCases(0).document.get.documentBytes
    assertEquals(9, bytes.length)
    ts.runOneTest("testNilCompare")
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
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
    val actual = doc.documentBytes.toList
    val expected = List(0x02).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_utf8_1char() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst">1</documentPart>
              </document>

    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: TextDocumentPart => x }
    val firstPart = dp(0)
    val actual = doc.documentBytes.toList
    val expected = List(0x31).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_7bit_1char() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="us-ascii-7-bit-packed">1</documentPart>
              </document>

    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: TextDocumentPart => x }
    val firstPart = dp(0)
    val actual = doc.documentBytes.toList
    val expected = List(0x31).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB3_7bit_2char() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="us-ascii-7-bit-packed">12</documentPart>
              </document>

    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: TextDocumentPart => x }
    val firstPart = dp(0)
    val actual = doc.documentBytes.toList
    val expected = List(0x31, 0x19).map { _.toByte }
    assertEquals(expected, actual)
  }

  @Test def testLSB4() {
    val xml = <document>
                <documentPart type="bits" bitOrder="LSBFirst">00110011 110</documentPart>
              </document>

    val doc = new Document(xml, null)
    val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
    val firstPart = dp(0)
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
      val dp = doc.documentParts.collect { case x: BitsDocumentPart => x }
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
                <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst">abcdefgh</documentPart>
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
                <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst">abc</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">111 1111</documentPart>
                <documentPart type="bits" bitOrder="MSBFirst">1</documentPart>
              </document>
    val exc = intercept[Exception] {
      val doc = new Document(xml, null)
      val dp = doc.documentParts.collect { case x: DocumentPart => x }
    }
    assertTrue(exc.getMessage().contains("bitOrder"))
  }

}
