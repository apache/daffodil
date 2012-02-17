package daffodil.tdml

/**
 * Copyright (C) 2012, Tresys Technologies LLC., All rights reserved.
 */

import scala.xml._
import daffodil.exceptions.Assert

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

/**
 * Parses and runs tests expressed in IBM's tdml "Test Data Markup Language"
 */

//
// TODO: add ability to embed the schema directly in the TDML file for a 100% self-contained test case. Note that
// the schemas should not be inside the individual test cases, but instead should be separate top-level structures 
// referenced from the test cases.
//
// TODO: validate the infoset XML against the DFDL Schema, that is using it as an XML Schema for the infoset.
// This would prevent errors where the infoset instance and the schema drift apart under maintenance.
//
// TODO: Unparser variant. Inverts the whole thing by starting from the infoset, and constructing a document.
// 

case class DFDLTestSuite(ts: NodeSeq) {
  lazy val parserTestCases = (ts \ "parserTestCase").map { node => new ParserTestCase(node, this) }
  lazy val suiteName = (ts \ "@suiteName").text
  lazy val description = (ts \ "@description").text
}

case class ParserTestCase(ptc: NodeSeq, parent: DFDLTestSuite) {
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
  // TODO: Or better, validate this in the XML Schema for tdml
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
    val expected = <byte1 xmlns:xsi={ XSIns }  xmlns:xs={ XSDns } xsi:type="xs:byte">123</byte1>
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

}