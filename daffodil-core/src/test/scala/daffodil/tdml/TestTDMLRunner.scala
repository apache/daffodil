package daffodil.tdml

import java.io.File

import scala.Array.canBuildFrom
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility
import scala.xml.XML

import org.scalatest.junit.JUnit3Suite

import daffodil.Implicits.using
import daffodil.compiler.Compiler
import daffodil.xml.XMLUtils
import daffodil.util.Validator
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import daffodil.util._

class TestTDMLRunner extends JUnit3Suite {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  def testDocPart1() {
    val xml = <documentPart type="text">abcde</documentPart>
    val dp = new DocumentPart(xml, null)
    val actual = dp.convertedContent
    val expected = Vector('a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte, 'e'.toByte)
    assertEquals(expected, actual)
  }

  def testDocPart2() {
    val xml = <documentPart type="byte">123abc</documentPart>
    val dp = new DocumentPart(xml, null)
    val hexDigits = dp.hexDigits
    assertEquals("123abc", hexDigits)
    val actual = dp.convertedContent
    val expected = Vector(0x12, 0x3a, 0xbc).map { _.toByte }
    assertEquals(expected, actual)
  }

  def testDocPart3() {
    val xml = <document>
                <documentPart type="byte">12</documentPart>
                <documentPart type="byte">3abc</documentPart>
              </document>

    val doc = new Document(xml, null)
    val firstPart = doc.documentParts(0)
    val secondPart = doc.documentParts(1)
    assertEquals("12", firstPart.hexDigits)
    assertEquals("3abc", secondPart.hexDigits)
    val actual = doc.documentBytes
    val expected = Vector(0x12, 0x3a, 0xbc).map { _.toByte }
    assertEquals(expected, actual)
  }

  def testDocWithMultiByteUnicode() {
    val xml = <document>
                <documentPart type="text">&#x24;&#xA2;&#x20AC;&#x2028;</documentPart>
              </document>
    val doc = new Document(xml, null)
    val docPart = doc.documentParts(0)
    val bytes = docPart.convertedContent
    assertEquals(9, bytes.length)
    val str = docPart.partRawContent // a string
    assertEquals(4, str.length)
    assertEquals("$¢€\u2028", str)
  }

  def test1() {
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

    lazy val ts = new DFDLTestSuite(xml)
    val ptc = ts.parserTestCases(0)
    assertEquals("test1", ptc.name)
    assertEquals("byte1", ptc.root)
    assertEquals("test-suite/ibm-contributed/dpanum.dfdl.xsd", ptc.model)
    assertTrue(ptc.description.contains("Some test case description."))
    val doc = ptc.document.get
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte)
    val actualBytes = doc.documentBytes
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset.get
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1 xmlns:xsi={ xsi } xmlns:xs={ xsd }>123</byte1>
    assertEquals(expected, trimmed)
  }

  def test2() {
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
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte)
    val actualBytes = doc.documentBytes
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset.get
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1 xmlns:xsi={ xsi } xmlns:xs={ xsd }>123</byte1>
    assertEquals(expected, trimmed)
  }

  val testSchema = TestUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)

  // @Test
  def testTDMLParseSuccess() {

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
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLParseSuccess", Some(testSchema))
  }

  def testTDMLParseDetectsErrorWithSpecificMessage() {

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

  def testTDMLParseDetectsErrorWithPartMessage() {

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

  def testTDMLParseDetectsErrorAnyMessage() {

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

  def testTDMLParseDetectsNoError() {

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
//  def testTDMLParseDetectsNoWarning() {
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

  // @Test
  def testTDMLParseRunAll() {
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

  def testRunModelFile() {
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

  def testRunTDMLFileReferencingModelFile() {
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

  def testFindModelFile() {
    lazy val ts = new DFDLTestSuite(new File("daffodil-core/src/test/resources/test-suite/ibm-contributed/dpaext1.tdml"))
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

  // @Test
  def testEmbeddedSchemaWorks() {
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

  // @Test
  def testEmbeddedSchemaValidates() {
    val testSuite = tdmlWithEmbeddedSchemaInvalid
    val exc = intercept[Exception] {
      lazy val ts = new DFDLTestSuite(testSuite)
      ts.isTDMLFileValid
    }
    val msg = exc.getMessage
    // println(msg)
    assertTrue(msg.contains("notAllowed"))
  }

  def testRunTDMLSelfContainedFile() {
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

  def testTDMLSelfContainedFileValidates() {
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = tdmlWithEmbeddedSchemaInvalid
    try {
      using(new java.io.FileWriter(tmpTDMLFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }
      val exc = intercept[Exception] {
        lazy val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
        ts.isTDMLFileValid
      }
      val msg = exc.getMessage
      // println(msg)
      assertTrue(msg.contains("notAllowed"))

    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
  }

  def testTDMLUnparse() {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } xmlns:tns={ tns } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } suiteName="theSuiteName">
                      <ts:defineSchema name="unparseTestSchema1">
                        <dfdl:format ref="tns:daffodilTest1"/>
                        <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 9 }"/>
                      </ts:defineSchema>
                      <ts:serializerTestCase ID="some identifier" name="testTDMLUnparse" root="data" model="unparseTestSchema1">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>123456789</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>123456789</ts:document>
                      </ts:serializerTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLUnparse")
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

  // @Test
  def testMultiByteUnicodeWorks() {
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

  // @Test
  def testMultiByteUnicodeWithCDATAWorks() {
    val testSuite = tdmlWithUnicode5E74AndCDATA
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testMultiByteUnicodeWithCDATAWorks")
  }
  
  def testBits() {
    val bits = new DocumentPart(<document-part type="bits">111</document-part>, null)
    assertEquals("111", bits.bitDigits)
    val bytes = bits.bitContentToBytes.toList
    assertEquals(7, bytes(0))
  }

}
