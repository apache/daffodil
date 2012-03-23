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
import daffodil.xml.XMLUtil
import daffodil.util.Validator
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue


class TestTDMLRunner extends JUnit3Suite {

  val tdml = XMLUtil.TDML_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val xsd = XMLUtil.XSD_NAMESPACE
  val example = XMLUtil.EXAMPLE_NAMESPACE
  val sub = XMLUtil.DFDL_XMLSCHEMASUBSET_NAMESPACE

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
    val doc = ptc.document
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte)
    val actualBytes = doc.documentBytes
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1 xmlns:xsi={ xsi } xmlns:xs={ xsd }>123</byte1>
    assertEquals(expected, trimmed)
  }

  def test2() {
    val xml = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                <parserTestCase  ID="some identifier" name="firstUnitTest" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd">
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
    val doc = ptc.document
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte)
    val actualBytes = doc.documentBytes
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1 xmlns:xsi={ xsi } xmlns:xs={ xsd }>123</byte1>
    assertEquals(expected, trimmed)
  }

  // @Test
  def testTDMLrunOne() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" 
dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" 
dfdl:documentFinalTerminatorCanBeMissing="yes"
dfdl:textNumberRep="standard" dfdl:emptyValueDelimiterPolicy="none" dfdl:initiator=""/>
      </schema>
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:parserTestCase  ID="some identifier" name="firstUnitTest" root="data">
                        <ts:document>37\n</ts:document>
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

  
  // @Test
  def testTDMLrunAll() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ tdml } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"
dfdl:textNumberRep="standard" dfdl:emptyValueDelimiterPolicy="none" dfdl:initiator=""/>
      </schema>
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="firstUnitTest" root="data">
                        <document>37\n</document>
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
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:initiator="" dfdl:textNumberRep="standard" dfdl:emptyValueDelimiterPolicy="none" dfdl:terminator="%NL;" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="firstUnitTest" root="data" model={ tmpFileName }>
                        <document>37\n</document>
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
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:initiator="" dfdl:textNumberRep="standard" dfdl:emptyValueDelimiterPolicy="none" dfdl:terminator="%NL;" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunTDMLFile" root="data" model={ tmpFileName }>
                        <document>37\n</document>
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
  
//  TODO: Someday, make self-contained test files work.
//  Warning - tried this. It seems pretty hard to get working. Eclipse validation 
//  just doesn't seem to be able to cope.
//
//  // @Test
//  def testEmbeddedSchemaWorks() {
//    val testSuite = <tdml:testSuite  suiteName="theSuiteName" xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
//                      <tdml:defineSchema name="mySchema">
//                        <schema xmlns={ sub } targetNamespace={ example } xmlns:tns={ example } >
//                          <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
//                        </schema>
//                      </tdml:defineSchema>
//                      <parserTestCase xmlns={ tdml } name="firstUnitTest" root="data" model="mySchema">
//                        <document>37\n</document>
//                        <infoset>
//                          <dfdlInfoset>
//                            <data xmlns={ example }>37</data>
//                          </dfdlInfoset>
//                        </infoset>
//                      </parserTestCase>
//                    </tdml:testSuite>
//    val ts = new DFDLTestSuite(testSuite)
//    ts.runOneTest("firstUnitTest")
//  }

//  def testRunTDMLSelfContainedFile() {
//    val tmpTDMLFileName = getClass.getName() + ".tdml"
//    val testSuite = <tdml:testSuite suiteName="theSuiteName" xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
//                      <tdml:defineSchema name="mySchema">
//                        <schema xmlns={ xsd } targetNamespace={ example } >
//                          <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
//                        </schema>
//                      </tdml:defineSchema>
//                      <parserTestCase xmlns={ tdml } name="testRunTDMLFile" root="data" model="mySchema">
//                        <document>37\n</document>
//                        <infoset>
//                          <dfdlInfoset>
//                            <data xmlns={ example }>37</data>
//                          </dfdlInfoset>
//                        </infoset>
//                      </parserTestCase>
//                    </tdml:testSuite>
//    try {
//      using(new java.io.FileWriter(tmpTDMLFileName)) {
//        fw =>
//          fw.write(testSuite.toString())
//      }
//      val ts = new DFDLTestSuite(new java.io.File(tmpTDMLFileName))
//      ts.runAllTests()
//    } finally {
//      val t = new java.io.File(tmpTDMLFileName)
//      t.delete()
//    }
//  }

}
