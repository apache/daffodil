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
import daffodil.dsom.Compiler
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
                <parserTestCase name="firstUnitTest" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd" description="Some test case description.">
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
    assertEquals("firstUnitTest", ptc.name)
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
                <parserTestCase ID="some identifier" name="firstUnitTest" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd">
                  <document>0123</document>
                  <infoset>
                    <dfdlInfoset xmlns:xs={ xsd } xmlns:xsi={ xsi }>
                      <byte1>123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>

    val ts = new DFDLTestSuite(xml)
    val tsn = ts.suiteName
    assertEquals("theSuiteName", tsn)
    assertEquals("", ts.description)
    val ptc = ts.parserTestCases(0)
    assertEquals("", ptc.description)
    assertEquals("firstUnitTest", ptc.name)
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
                      <ts:parserTestCase ID="some identifier" name="firstUnitTest" root="data">
                        <ts:document>37</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("firstUnitTest", Some(testSchema))
  }

  def testTDMLParseDetectsErrorWithSpecificMessage() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="firstUnitTest" root="data">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error>AA</ts:error><!-- can have several substrings of message -->
                          <ts:error>xs:int</ts:error><!-- all are checked against the message -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("firstUnitTest", Some(testSchema))
  }

  def testTDMLParseDetectsErrorWithPartMessage() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="firstUnitTest" root="data">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error>AA</ts:error>
                          <ts:error>xs:float</ts:error><!-- Detect this mismatch. It will say xs:int -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    val exc = intercept[Exception] {
      ts.runOneTest("firstUnitTest", Some(testSchema))
    }
    assertTrue(exc.getMessage().contains("""message "xs:float""""))
  }

  def testTDMLParseDetectsErrorAnyMessage() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="firstUnitTest" root="data">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error/><!-- don't care what message is -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("firstUnitTest", Some(testSchema))
  }

  def testTDMLParseDetectsNoError() {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase ID="some identifier" name="firstUnitTest" root="data">
                        <ts:document>37</ts:document>
                        <ts:errors>
                          <ts:error/><!-- don't care what message is -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    val exc = intercept[Exception] {
      ts.runOneTest("firstUnitTest", Some(testSchema))
    }
    println(exc)
    assertTrue(exc.getMessage().contains("Expected error"))
  }

// TODO: Implement Warnings
//
//  def testTDMLParseDetectsNoWarning() {
//
//    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
//                      <ts:parserTestCase ID="some identifier" name="firstUnitTest" root="data">
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
//    val ts = new DFDLTestSuite(testSuite)
//    val exc = intercept[Exception] {
//      ts.runOneTest("firstUnitTest", Some(testSchema))
//    }
//    assertTrue(exc.getMessage().contains("Did not find"))
//  }

  // @Test
  def testTDMLParseRunAll() {
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="firstUnitTest" root="data">
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                      <parserTestCase name="firstUnitTest" root="data">
                        <document>37\n</document>
                        <infoset>
                          <dfdlInfoset xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                            <data>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>
    val ts = new DFDLTestSuite(testSuite)
    ts.runAllTests(Some(testSchema))
  }

  def testRunModelFile() {
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="firstUnitTest" root="data" model={ tmpFileName }>
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
      val ts = new DFDLTestSuite(testSuite)
      ts.runOneTest("firstUnitTest")
    } finally {
      val f = new java.io.File(tmpFileName)
      f.delete()
    }
  }

  def testRunTDMLFileReferencingModelFile() {
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunTDMLFile" root="data" model={ tmpFileName }>
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
      val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
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
    val ts = new DFDLTestSuite(new File("./test-suite/ibm-contributed/dpaext1.tdml"))
    val mf = ts.findModelFile("./fvt/ext/dpa/dpaspc121_01.dfdl.xsd")
    assertTrue(mf.exists())
  }

  val tdmlWithEmbeddedSchema =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="firstUnitTest" root="data" model="mySchema">
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
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("firstUnitTest")
  }

  val tdmlWithEmbeddedSchemaInvalid =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="notAllowed" dfdl:notAProp="{ 2 }"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="firstUnitTest" root="data" model="mySchema">
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
      val ts = new DFDLTestSuite(testSuite)
      ts.isTDMLFileValid
    }
    val msg = exc.getMessage
    println(msg)
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
      val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
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
        val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
        ts.isTDMLFileValid
      }
      val msg = exc.getMessage
      println(msg)
      assertTrue(msg.contains("notAllowed"))

    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
  }

  def testTDMLSerializeDetectsError() {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:serializerTestCase ID="some identifier" name="firstUnitTest" root="data">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document></ts:document><!-- should fail with no data being output -->
                        <ts:errors>
                          <ts:error>not yet implemented</ts:error>
                        </ts:errors>
                      </ts:serializerTestCase>
                    </ts:testSuite>
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("firstUnitTest", Some(testSchema))
  }

  val tdmlWithUnicode2028 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xsd:element name="data" type="xsd:string" dfdl:encoding="utf-8" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="firstUnitTest" root="data" model="mySchema">
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
    val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("firstUnitTest")
  }

}
