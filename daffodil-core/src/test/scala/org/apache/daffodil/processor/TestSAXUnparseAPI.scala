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

package org.apache.daffodil.processor

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.xml.DaffodilSAXParserFactory
import org.apache.daffodil.xml.XMLUtils
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.InputSource
import org.xml.sax.SAXParseException
import org.xml.sax.XMLReader

class TestSAXUnparseAPI {
  import TestSAXUtils._

  /**
   * tests the base case of unparsing error free using SAX. Default for namespace features/prefixes
   * is true/true for SAXParserFactory
   */
  @Test def testUnparseContentHandler_unparse(): Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val bai = new ByteArrayInputStream(testInfosetString.getBytes)
    xmlReader.parse(new InputSource(bai))
    val ur = unparseContentHandler.getUnparseResult
    assertTrue(!ur.isError)
    assertEquals(testData, bao.toString)
  }

  /**
   * Test the case when a user supplies 0 as the batch size as soon as an
   * invalid tunable is set. Minimum batchsize must be 1.
   */
  @Test def testUnparseContentHandler_unparse_saxUnparseEventBatchSize_0(): Unit = {
     val e = intercept[java.lang.IllegalArgumentException] {
      testDataProcessor(testSchema1, Map("saxUnparseEventBatchSize" -> "0"))
     }
     val eMsg = e.getMessage
     assertTrue(eMsg.contains("saxUnparseEventBatchSize"))
     assertTrue(eMsg.contains("0"))
  }

  /**
   * tests the case of unparsing with the namespace features/prefixes set to true/false
   */
  @Test def testUnparseContentHandler_unparse_namespace_feature(): Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser().getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, false)
    val bai = new ByteArrayInputStream(testInfosetString.getBytes)
    xmlReader.parse(new InputSource(bai))
    val ur = unparseContentHandler.getUnparseResult
    assertTrue(!ur.isError)
    assertEquals(testData, bao.toString)
  }

  /**
   * tests the case of unparsing with the namespace features/prefixes set to false/true
   */
  @Test def testUnparseContentHandler_unparse_namespace_prefix_feature(): Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, false)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val bai = new ByteArrayInputStream(testInfosetString.getBytes)
    xmlReader.parse(new InputSource(bai))
    val ur = unparseContentHandler.getUnparseResult
    assertTrue(!ur.isError)
    assertEquals(testData, bao.toString)
  }

  /**
   * Verifies if the XML reader has secure defaults, then unparsing does
   * detect and refuse the use of DOCTYPE decls.
   *
   * This is part of fixing DAFFODIL-1422, DAFFODIL-1659 - disallow doctype decls.
   */
  @Test def testUnparse_NoDocType_feature(): Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, false)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val xmlWithDocType = """<?xml version="1.0" ?>
      <!DOCTYPE root_element [
        Document Type Definition (DTD):
        elements/attributes/entities/notations/
          processing instructions/comments/PE references
    ]>
    <list xmlns="http://example.com"><w>9</w><w>1</w><w>0</w></list>
    """
    val bai = new ByteArrayInputStream(xmlWithDocType.getBytes)
    val e = intercept[SAXParseException] {
      xmlReader.parse(new InputSource(bai))
    }
    val m = e.getMessage()
    assertTrue(m.contains("DOCTYPE is disallowed"))
  }
}
