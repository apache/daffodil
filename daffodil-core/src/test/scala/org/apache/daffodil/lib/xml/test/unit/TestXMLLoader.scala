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

package org.apache.daffodil.lib.xml.test.unit

import scala.collection.mutable.ArrayBuffer
import scala.xml.SAXParseException

import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.iapi.StringSchemaSource
import org.apache.daffodil.lib.xml.DaffodilXMLLoader

import org.junit.Assert._
import org.junit.Test

class TestXMLLoader {

  @Test
  def test_schemaLoad(): Unit = {
    val data =
      """<?xml version="1.0"?>
        |<xs:schema
        |targetNamespace="http://example.com"
        |xmlns:ex="http://example.com"
        |xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
        |  <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        |  <xs:annotation>
        |    <xs:appinfo source="http://www.ogf.org/dfdl/">
        |      <dfdl:format lengthKind="delimited" ref="ex:GeneralFormat"/>
        |    </xs:appinfo>
        |  </xs:annotation>
        |  <xs:element name="e1">
        |    <xs:complexType>
        |      <xs:sequence>
        |        <xs:element name="s1" type="xs:int"/>
        |      </xs:sequence>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>
        |""".stripMargin
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(data)
    val root =
      loader.load(ss, None, false)
    assertEquals("http://example.com", (root \ "@targetNamespace").text)
  }

  @Test
  def test_startsWithPINotProlog(): Unit = {
    val data =
      """<?xml-model href="../Schematron/iCalendar.sch" type="application/xml" schematypens="http://purl.oclc.org/dsdl/schematron"?>
        |<xs:schema
        |targetNamespace="http://example.com"
        |xmlns:ex="http://example.com"
        |xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
        |  <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        |  <xs:annotation>
        |    <xs:appinfo source="http://www.ogf.org/dfdl/">
        |      <dfdl:format lengthKind="delimited" ref="ex:GeneralFormat"/>
        |    </xs:appinfo>
        |  </xs:annotation>
        |  <xs:element name="e1">
        |    <xs:complexType>
        |      <xs:sequence>
        |        <xs:element name="s1" type="xs:int"/>
        |      </xs:sequence>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>
        |""".stripMargin
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(data)
    val root =
      loader.load(ss, None, false)
    assertEquals("http://example.com", (root \ "@targetNamespace").text)
  }

  /**
   * Characterize behavior of scala's xml loader w.r.t. CDATA preservation.
   *
   * At the time of this testing. The basic scala xml loader uses Xerces (java)
   * under the hood. It seems hopelessly broken w.r.t CDATA region preservation.
   *
   * This bug was fixed in Scala XML 2.1.0. The XML.loadString function now
   * creates CDATA object.
   */
  @Test def test_scala_loader_cdata_bug(): Unit = {

    val data = "<x><![CDATA[a\nb&\"<>]]></x>"
    val node = scala.xml.XML.loadString(data)
    val xbody = node.child
    assertEquals(1, xbody.length)
    val body = xbody(0)
    val txt = body.text
    assertTrue(txt.contains("a"))
    assertTrue(txt.contains("b"))
    assertFalse(
      txt.contains("<![CDATA[")
    ) // correct, PCData.text returns only the contents, not the brackets
    assertFalse(
      txt.contains("]]>")
    ) // correct, PCData.text returns only the contents, not the brackets
    assertTrue(txt.contains("a\nb")) // they preserve the contents
    assertTrue(body.isInstanceOf[scala.xml.PCData])
    assertTrue(txt.contains("""&"<>""")) // They get the right characters in there.
    assertTrue(body.toString.contains("""&"<>""")) // body.toString preserves content
    assertTrue(body.toString.contains("<![CDATA[a\nb&\"<>]]>"))

  }

  /**
   * Verify that we don't accept doctype decls in our XML.
   *
   * Part of fixing DAFFODIL-1422, DAFFODIL-1659.
   */
  @Test
  def testNoDoctypeAllowed(): Unit = {

    val data = """<?xml version="1.0" ?>
      <!DOCTYPE root_element [
        Document Type Definition (DTD):
        elements/attributes/entities/notations/
          processing instructions/comments/PE references
    ]>
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"/>
    """
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(data)
    val e = intercept[SAXParseException] {
      loader.load(ss, None, false, true)
    }
    val m = e.getMessage()
    assertTrue(m.contains("DOCTYPE"))
  }

  /**
   * Test shows that a CRLF in the middle of XML data can
   * be preserved or normalized by loader options.
   */
  @Test
  def testDaffodilXMLLoaderCRLFNormalization(): Unit = {
    // This data has an embedded CRLF, and an embedded isolated CR
    val xmlTextWithCRLFs =
      """<?xml version="1.0" ?>""" + "\r\n" +
        "<data>before" + "\r\n" + // regular CRLF
        "after" + "\r" + // isolated CR
        "end<![CDATA[CDATAbefore" + "\r\n" + // CRLF inside CDATA
        "after" + "\r" + // isolated CR inside CDATA
        "endCDATA]]></data>" + "\r\n"
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(xmlTextWithCRLFs)
    //
    // Our loader preserves CDATA regions. They stay as separate Nodes (of type PCDATA)
    // and toString will print them out into the text with the <![CDATA[...]]> preserved.
    //
    val xmlFromDafLoaderNonNormalized =
      loader.load(ss, None, addPositionAttributes = false, normalizeCRLFtoLF = false)
    val xmlFromDafLoaderNormalized =
      loader.load(ss, None, addPositionAttributes = false, normalizeCRLFtoLF = true)

    {
      // compare to the regular scala XML loader
      // Note that this doesn't preserve the CDATA/PCData
      val xmlFromScalaLoader = scala.xml.XML.loadString(xmlTextWithCRLFs)

      // calling .text on a Node creates a string, but also replaces PCData nodes
      // (all Atoms actually) with their contents. So if we call .text on our loader's
      // Node, it will be the same as the one from the regular scala xml loader.
      assertEquals(
        xmlFromDafLoaderNormalized.text,
        xmlFromScalaLoader.text
      ) // both have LF only
      assertEquals("before\nafter\nendCDATAbefore\nafter\nendCDATA", xmlFromScalaLoader.text)
    }

    assertEquals( // retains CRLF and CR
      "<data>before\r\nafter\rend<![CDATA[CDATAbefore\r\nafter\rendCDATA]]></data>",
      xmlFromDafLoaderNonNormalized.toString
    )
    assertEquals( // Converts CRLF/CR => LF
      "<data>before\nafter\nend<![CDATA[CDATAbefore\nafter\nendCDATA]]></data>",
      xmlFromDafLoaderNormalized.toString
    )
    assertEquals( // retains CRLF and CR, but eliminates CDATA brackets.
      "before\r\nafter\rendCDATAbefore\r\nafter\rendCDATA",
      xmlFromDafLoaderNonNormalized.text
    )
    assertEquals( // Converts CRLF/CR => LF, but elimintes CDATA brackets.
      "before\nafter\nendCDATAbefore\nafter\nendCDATA",
      xmlFromDafLoaderNormalized.text
    )

  }

  @Test def testLoaderToleratesXMLWithLeadingWhitespace(): Unit = {
    val xmlTextWithLeadingWhitespace = "    \n" + "<data>foo</data>\n"
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(xmlTextWithLeadingWhitespace)
    val xml = loader.load(ss, None, addPositionAttributes = false)
    assertEquals("foo", xml.text)
  }

  @Test def testLoaderToleratesXMLWithLeadingComments(): Unit = {
    val xmlText = "    \n" +
      "  <!-- a comment -->  \n  <data>foo</data>\n"
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(xmlText)
    val xml = loader.load(ss, None, addPositionAttributes = false)
    assertEquals("foo", xml.text)
  }

  @Test def testLoaderToleratesXMLWithPI(): Unit = {
    val xmlText = "    \n" +
      "<?aProcInstr yadda yadda ?>\n" +
      "  <!-- a comment -->  \n  <data>foo</data>\n"
    val loader = new DaffodilXMLLoader()
    val ss = StringSchemaSource(xmlText)
    val xml = loader.load(ss, None, addPositionAttributes = false)
    assertEquals("foo", xml.text)
  }

  @Test def testLoaderCatchesVarousBadXML(): Unit = {
    val xmlText = "    \n" + // no prolog some whitespace (tolerated)
      "&AnEntityRef;\n" + // entity refs not allowed
      "random text\n" + // just text not allowed
      "<data>foo</data>\n" +
      "<!-- comment afterwards --><another>element</another>\n&AnotherEntityRef;\nmore random text\n" // other bad stuff.
    val teh = new TestErrorHandler()
    val loader = new DaffodilXMLLoader(teh)
    val ss = StringSchemaSource(xmlText)
    val xml = loader.load(ss, None, addPositionAttributes = false)
    val msgs = teh.exceptions.map { _.getMessage() }.mkString("\n")
    println(msgs)
    assertTrue(msgs.contains("non-empty text nodes not allowed"))
    assertTrue(msgs.contains("random text"))
    assertTrue(msgs.contains("more random text"))
    assertTrue(msgs.contains("exactly one element"))
  }
}

class TestErrorHandler extends org.xml.sax.ErrorHandler {

  val exceptions = new ArrayBuffer[SAXParseException]

  def warning(exception: SAXParseException) = {
    exceptions += exception
  }

  def error(exception: SAXParseException) = {
    exceptions += exception
  }

  def fatalError(exception: SAXParseException) = {
    exceptions += exception
  }
}
