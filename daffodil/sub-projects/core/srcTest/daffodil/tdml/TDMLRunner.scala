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
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue

/**
 * Parses and runs tests expressed in IBM's contributed tdml "Test Data Markup Language"
 */

//
// TODO: plug in Daffodil API to run the tests.
//
// TODO: validate the TDML with the schema for TDML. (Temporarily just do this in eclipse using its validator)
//
// TODO: validate the infoset XML (expected result) against the DFDL Schema, that is using it as an XML Schema
// for the infoset. This would prevent errors where the infoset instance and the schema drift apart under maintenance.
//
// TODO: validate the actual result against the DFDL Schema using it as an XML Schema. 
//
// TODO: add ability to embed the schema directly in the TDML file for a 100% self-contained test case. Note that
// the schemas should not be inside the individual test cases, but instead should be separate top-level structures 
// referenced from the test cases.
//
// TODO: Unparser variant. Inverts the whole thing by starting from the infoset, and constructing a document.
// 

/**
 * TDML test suite runner
 *
 * Keep this independent of Daffodil, so that it can be used to run tests against other DFDL implementations as well.
 * E.g., it should only need an API specified as a collection of Scala traits, and some simple way to inject
 * dependency on one factory to create processors.
 */

class DFDLTestSuite(ts: NodeSeq, val tdmlFile: File = null) {

  def this(tdmlFile: File) = this(XML.loadFile(tdmlFile), tdmlFile)

  lazy val parserTestCases = (ts \ "parserTestCase").map { node => new ParserTestCase(node, this) }
  lazy val suiteName = (ts \ "@suiteName").text
  lazy val description = (ts \ "@description").text

  def runAllTests(schema: Option[Node] = None) {
    parserTestCases.map { _.run(schema) }
  }

  def runOneTest(testName: String, schema: Option[Node] = None) {
    val testCase = parserTestCases.find(_.name == testName)
    testCase match {
      case None => throw new Exception("test " + testName + " was not found.")
      case Some(tc) => {
        tc.run(schema)
      }
    }
  }

  /**
   * Try a few possibilities to find the model/schema file.
   *
   * IBM's suites have funny model paths in them. We don't have that file structure,
   * so we look for the schema/model files in the working directory, and in the same
   * directory as the tdml file.
   */
  def findModelFile(fileName: String): File = {
    val firstTry = new File(fileName)
    if (firstTry.exists()) firstTry
    else {
      // try ignoring the directory part
      val parts = fileName.split("/")
      val filePart = parts.last
      val secondTry = new File(filePart)
      if (secondTry.exists()) secondTry
      else {
        val tdmlDir = tdmlFile.getParent()
        val path = tdmlDir + "/" + filePart
        val thirdTry = new File(path)
        if (thirdTry.exists()) thirdTry
        else {
          throw new Exception("Unable to find model file " + fileName + ".")
        }
      }
    }
  }

}

case class ParserTestCase(ptc: NodeSeq, val parent: DFDLTestSuite) {
  lazy val Seq(document) = (ptc \ "document").map { node => new Document(node, this) }
  lazy val Seq(infoset) = (ptc \ "infoset").map { node => new Infoset(node, this) }
  lazy val name = (ptc \ "@name").text
  lazy val root = (ptc \ "@root").text
  lazy val model = (ptc \ "@model").text
  lazy val description = (ptc \ "@description").text
  lazy val unsupported = (ptc \ "@unsupported").text match {
    case "true" => true
    case "false" => false
    case _ => false
  }

  def run(schema: Option[Node] = None) = {
    val sch = schema match {
      case Some(sch) => {
        if (model != "") throw new Exception("You supplied a model attribute, and a schema argument. Can't have both.")
        sch
      }
      case None => {
        if (model == "") throw new Exception("No model was found.")
        val schemaFile = parent.findModelFile(model)
        val schemaNode = XML.loadFile(schemaFile)
        schemaNode
      }
    }
    val compiler = Compiler()
    val parser = compiler.compile(sch).onPath("/")
    val data = document.input
    val actual = parser.parse(data)
    val trimmed = Utility.trim(actual)
    val expected = infoset.contents
    val expectedNoAttrs = XMLUtil.removeAttributes(expected)
    val actualNoAttrs = XMLUtil.removeAttributes(trimmed)
    assertEquals(expectedNoAttrs, actualNoAttrs)
    System.err.println("Test " + name + " passed.")
  }
}

sealed abstract class DocumentContentType
case object Text extends DocumentContentType
case object Byte extends DocumentContentType
// TODO: add a Bits type so one can do 0110 1101 0010 0000 and so forth.
// TODO: add capability to specify character set encoding into which text is to be converted (all UTF-8 currently)

case class Document(d: NodeSeq, parent: ParserTestCase) {
  lazy val realDocumentParts = (d \ "documentPart").map { node => new DocumentPart(node, this) }
  lazy val documentParts = realDocumentParts match {
    case Seq() => {
      val docPart = new DocumentPart(<documentPart type="text">{ d.text }</documentPart>, this)
      List(docPart)
    }
    case _ => realDocumentParts
  }
  lazy val documentBytes = documentParts.map { _.convertedContent }.flatten

  /**
   * this 'input' is the kind our parser's parse method expects.
   */
  lazy val input = {
    val bytes = documentBytes.toArray
    val inputStream = new java.io.ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

}

case class DocumentPart(part: Node, parent: Document) {
  lazy val partContentType = (part \ "@type").toString match {
    case "text" => Text
    case "byte" => Byte
  }
  lazy val partRawContent = part.child.text
  lazy val convertedContent: Seq[Byte] = partContentType match {
    case Text => partRawContent.getBytes
    case Byte => hexContentToBytes
  }

  lazy val hexContentToBytes = hex2Bytes(hexDigits)

  val validHexDigits = "0123456789abcdefABCDEF"

  // Note: anything that is not a valid hex digit is simply skipped
  // TODO: we should check for whitespace and other characters we want to allow, and verify them.
  // TODO: Or better, validate this in the XML Schema for tdml via a pattern facet
  // TODO: Consider whether to support a comment syntax. When showing data examples this may be useful.
  //
  lazy val hexDigits = partRawContent.flatMap { ch => if (validHexDigits.contains(ch)) List(ch) else Nil }

  def hex2Bytes(hex: String): Array[Byte] = {
    (for { i <- 0 to hex.length - 1 by 2 if i > 0 || !hex.startsWith("0x") }
      yield hex.substring(i, i + 2))
      .map(Integer.parseInt(_, 16).toByte).toArray
  }

  def bytes2Hex(bytes: Array[Byte]): String = {
    def cvtByte(b: Byte): String = {
      (if ((b & 0xff) < 0x10) "0" else "") + java.lang.Long.toString(b & 0xff, 16)
    }

    "0x" + bytes.map(cvtByte(_)).mkString.toUpperCase
  }
}

case class Infoset(i: NodeSeq, parent: ParserTestCase) {
  lazy val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => new DFDLInfoset(Utility.trim(node), this) }
  lazy val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di: Node, parent: Infoset) {
  lazy val Seq(contents) = di.child // must be exactly one root element in here.
}

class TestTDMLRunner extends JUnit3Suite {

  val XSIns = "http://www.w3.org/2001/XMLSchema-instance"
  val XSDns = "http://www.w3.org/2001/XMLSchema"

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
    val xml = <testSuite suiteName="theSuiteName" description="Some Test Suite Description">
                <parserTestCase name="firstUnitTest" root="byte1" model="dpanum.dfdl.xsd" description="Some test case description.">
                  <document>0123</document>
                  <infoset>
                    <dfdlInfoset xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                      <byte1 xsi:type="xs:byte">123</byte1>
                    </dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>

    val ts = new DFDLTestSuite(xml)
    val ptc = ts.parserTestCases(0)
    assertEquals("firstUnitTest", ptc.name)
    assertEquals("byte1", ptc.root)
    assertEquals("dpanum.dfdl.xsd", ptc.model)
    assertTrue(ptc.description.contains("Some test case description."))
    val doc = ptc.document
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte)
    val actualBytes = doc.documentBytes
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1 xmlns:xsi={ XSIns } xmlns:xs={ XSDns } xsi:type="xs:byte">123</byte1>
    assertEquals(expected, trimmed)
  }

  def test2() {
    val xml = <testSuite suiteName="theSuiteName">
                <parserTestCase name="firstUnitTest" root="byte1" model="dpanum.dfdl.xsd">
                  <document>0123</document>
                  <infoset>
                    <dfdlInfoset xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                      <byte1 xsi:type="xs:byte">123</byte1>
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
    assertEquals("dpanum.dfdl.xsd", ptc.model)
    val doc = ptc.document
    val expectedBytes = Vector('0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte)
    val actualBytes = doc.documentBytes
    assertEquals(expectedBytes, actualBytes)
    val infoset = ptc.infoset
    val actualContent = infoset.dfdlInfoset.contents
    val trimmed = actualContent
    val expected = <byte1 xmlns:xsi={ XSIns } xmlns:xs={ XSDns } xsi:type="xs:byte">123</byte1>
    assertEquals(expected, trimmed)
  }

  // @Test
  def testTDMLrunOne() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val tdml = <testSuite suiteName="theSuiteName">
                 <parserTestCase name="firstUnitTest" root="data">
                   <document>37\n</document>
                   <infoset>
                     <dfdlInfoset xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                       <data xmlns="http://example.com">37</data>
                     </dfdlInfoset>
                   </infoset>
                 </parserTestCase>
               </testSuite>
    val ts = new DFDLTestSuite(tdml)
    ts.runOneTest("firstUnitTest", Some(testSchema))
  }

  // @Test
  def testTDMLrunAll() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val tdml = <testSuite suiteName="theSuiteName">
                 <parserTestCase name="firstUnitTest" root="data">
                   <document>37\n</document>
                   <infoset>
                     <dfdlInfoset xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                       <data xmlns="http://example.com">37</data>
                     </dfdlInfoset>
                   </infoset>
                 </parserTestCase>
                 <parserTestCase name="firstUnitTest" root="data">
                   <document>37\n</document>
                   <infoset>
                     <dfdlInfoset xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                       <data xmlns="http://example.com">37</data>
                     </dfdlInfoset>
                   </infoset>
                 </parserTestCase>
               </testSuite>
    val ts = new DFDLTestSuite(tdml)
    ts.runAllTests(Some(testSchema))
  }

  def testRunModelFile() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val tdml = <testSuite suiteName="theSuiteName">
                 <parserTestCase name="firstUnitTest" root="data" model={ tmpFileName }>
                   <document>37\n</document>
                   <infoset>
                     <dfdlInfoset xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                       <data xmlns="http://example.com">37</data>
                     </dfdlInfoset>
                   </infoset>
                 </parserTestCase>
               </testSuite>
    try {
      using(new java.io.FileWriter(tmpFileName)) {
        fileWriter =>
          fileWriter.write(testSchema.toString())
      }
      val ts = new DFDLTestSuite(tdml)
      ts.runOneTest("firstUnitTest")
    } finally {
      val f = new java.io.File(tmpFileName)
      f.delete()
    }
  }

  def testRunTDMLFile() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val tmpFileName = getClass.getName() + ".dfdl.xsd"
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val tdml = <testSuite suiteName="theSuiteName">
                 <parserTestCase name="testRunTDMLFile" root="data" model={ tmpFileName }>
                   <document>37\n</document>
                   <infoset>
                     <dfdlInfoset xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                       <data xmlns="http://example.com">37</data>
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
          fw.write(tdml.toString())
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

}