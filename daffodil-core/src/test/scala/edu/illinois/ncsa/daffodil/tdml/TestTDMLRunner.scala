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
import org.scalatest.junit.JUnitSuite
import edu.illinois.ncsa.daffodil.Implicits.using
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.assertFalse
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._

class TestTDMLRunner extends JUnitSuite {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  @Test def testDocPart1() {
    val xml = <documentPart type="text">abcde</documentPart>
    val dp = new DocumentPart(xml, null)
    val actual = dp.textContentToBytes.toList
    val expected = Vector('a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte, 'e'.toByte).toList
    assertEquals(expected, actual)
  }

  @Test def testDocPart2() {
    val xml = <document><documentPart type="byte">123abc</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts(0)
    val hexDigits = dp.hexDigits
    assertEquals("123abc", hexDigits)
    val actual = doc.documentBytes.toList
    val expected = Vector(0x12, 0x3a, 0xbc).map { _.toByte }.toList
    assertEquals(expected, actual)
  }

  @Test def testHexOddNumberOfNibbles() {
    val xml = <document><documentPart type="byte">123</documentPart></document>
    val doc = new Document(xml, null)
    val dp = doc.documentParts(0)
    val hexDigits = dp.hexDigits
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
    val firstPart = doc.documentParts(0)
    val secondPart = doc.documentParts(1)
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
                <documentPart type="text">\%#x24;%#xA2;%#x20AC;%%%NUL;%LS;%#IGNORED;</documentPart>
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
    val xml = <document>
                <documentPart type="file">/test/tdml/test.txt</documentPart>
              </document>
    val doc = new Document(xml, null)
    val docPart = doc.documentParts(0)
    val bb = java.nio.ByteBuffer.allocate(11)
    doc.data.read(bb)
    val bytes = bb.array()
    val actual = new String(bytes.toArray, "UTF8")
    assertEquals("test\n1\n2\n3\n", actual)
  }

  @Test def testDocWithBinaryFile() {
    val xml = <document>
                <documentPart type="file">/test/tdml/test.bin</documentPart>
              </document>
    val doc = new Document(xml, null)
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

  val testSchema = TestUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)

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
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunModelFile" root="data" model={ tmpFileName }>
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
        fileWriter =>
          fileWriter.write(testSchema.toString())
      }
      lazy val ts = new DFDLTestSuite(testSuite)
      ts.runOneTest("testRunModelFile")
    } finally {
      val f = new java.io.File(tmpFileName)
      f.delete()
    }
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

  @Test def testFindModelFile() {
    lazy val res = Misc.getRequiredResource("/test-suite/ibm-contributed/dpaext1.tdml")
    lazy val ts = new DFDLTestSuite(new File(res.toURI()))
    val mf = ts.findModelFile("./fvt/ext/dpa/dpaspc121_01.dfdl.xsd")
    assertTrue(mf.exists())
  }

  val tdmlWithEmbeddedSchema =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
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
    <tdml:testSuite suiteName="testEmbeddedSchemaValidates" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
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
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
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
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
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
    assertEquals("11100000", doc.documentBitsFullBytes)
    val bytes = doc.documentBytes.toList
    assertEquals(-32, bytes(0))
    assertEquals(3, doc.nBits)
  }

  @Test def testNilCompare() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
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
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
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
}
