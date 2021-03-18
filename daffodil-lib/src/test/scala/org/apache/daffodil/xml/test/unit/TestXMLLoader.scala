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

package org.apache.daffodil.xml.test.unit

import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.api.StringSchemaSource
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.junit.Assert._
import org.junit.Test
import org.xml.sax.SAXParseException

class TestXMLLoader {

  /**
   * Characterize behavior of scala's xml loader w.r.t. CDATA preservation.
   *
   * At the time of this testing. The basic scala xml loader uses Xerces (java)
   * under the hood. It seems hopelessly broken w.r.t CDATA region preservation.
   */
  @Test def test_scala_loader_cdata_bug(): Unit = {

    val data = """<x><![CDATA[a
b&"<>]]></x>"""
    val node = scala.xml.XML.loadString(data)
    val <x>{ xbody @ _* }</x> = node
    assertEquals(1, xbody.length)
    val body = xbody(0)
    val txt = body.text
    assertTrue(txt.contains("a"))
    assertTrue(txt.contains("b"))
    //
    // Note to developer - whomewever sees this test failing....
    //
    // IF this test fails, it means that the scala xml loader have been FIXED (hooray!)
    // and our need for the ConstructingParser may have gone away.
    //
    assertFalse(txt.contains("<![CDATA[")) // wrong
    assertFalse(txt.contains("]]>")) // wrong
    assertTrue(txt.contains("a\nb")) // they preserve the contents
    assertFalse(body.isInstanceOf[scala.xml.PCData]) // wrong - they don't preserve the object properly.
    assertTrue(body.isInstanceOf[scala.xml.Text]) // wrong
    assertTrue(txt.contains("""&"<>""")) // They get the right characters in there.
    assertTrue(body.toString.contains("""&amp;&quot;&lt;&gt;""")) // wrong
    assertFalse(body.toString.contains("""<![CDATA[a
b&"<>]]>"""))

  }

  /**
   * Verify that we don't accept doctype decls in our XML.
   *
   * Part of fixing DAFFODIL-1422, DAFFODIL-1659.
   */
  @Test
  def testNoDoctypeAllowed() : Unit = {

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
    assertTrue(m.contains("DOCTYPE is disallowed"))
  }

  /**
   * Test shows that a CRLF in the middle of XML data can
   * be preserved or normalized by loader options.
   */
  @Test
  def testDaffodilXMLLoaderCRLFNormalization() : Unit = {
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
      assertEquals(xmlFromDafLoaderNormalized.text, xmlFromScalaLoader.text) // both have LF only
      assertEquals("before\nafter\nendCDATAbefore\nafter\nendCDATA", xmlFromScalaLoader.text)
    }

    assertEquals( // retains CRLF and CR
      "<data>before\r\nafter\rend<![CDATA[CDATAbefore\r\nafter\rendCDATA]]></data>",
      xmlFromDafLoaderNonNormalized.toString)
    assertEquals( // Converts CRLF/CR => LF
      "<data>before\nafter\nend<![CDATA[CDATAbefore\nafter\nendCDATA]]></data>",
      xmlFromDafLoaderNormalized.toString)
    assertEquals( // retains CRLF and CR, but eliminates CDATA brackets.
      "before\r\nafter\rendCDATAbefore\r\nafter\rendCDATA",
      xmlFromDafLoaderNonNormalized.text)
    assertEquals( // Converts CRLF/CR => LF, but elimintes CDATA brackets.
      "before\nafter\nendCDATAbefore\nafter\nendCDATA",
      xmlFromDafLoaderNormalized.text)

  }
}
