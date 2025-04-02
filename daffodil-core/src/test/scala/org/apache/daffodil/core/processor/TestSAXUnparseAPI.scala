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

package org.apache.daffodil.core.processor

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.xml.DaffodilSAXParserFactory
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnhandledSAXException

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.InputSource
import org.xml.sax.SAXException
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
   * tests the case of unparsing with the namespace features/prefixes set to
   * false/true, with non-empty prefixes and ignored attributes
   */
  @Test def testUnparseContentHandler_unparse_namespace_prefix_feature_non_empty_prefix()
    : Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, false)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val infoset =
      <p:list xmlns:p="http://example.com" ignored="attr"><p:w>9</p:w><p:w>1</p:w><p:w>0</p:w></p:list>
    val bai = new ByteArrayInputStream(infoset.toString.getBytes)
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
    // should be null since unparse never completed
    assertEquals(null, unparseContentHandler.getUnparseResult)
    val m = e.getMessage()
    assertTrue(m.contains("DOCTYPE is disallowed"))
  }

  /**
   * tests the case of unparsing with mixed content before a start element.
   */
  @Test def testUnparseContentHandler_unparse_mixed_01(): Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val mixedInfoset = <list xmlns="http://example.com">mixed<w>9</w><w>1</w><w>0</w></list>
    val bai = new ByteArrayInputStream(mixedInfoset.toString.getBytes)
    val e = intercept[SAXException] {
      xmlReader.parse(new InputSource(bai))
    }
    val m = e.getMessage()
    assertTrue(m.contains("Mixed content"))
    assertTrue(m.contains("prior to start"))
    assertTrue(m.contains("{http://example.com}w"))
  }

  /**
   * tests the case of unparsing with mixed content before an end element.
   */
  @Test def testUnparseContentHandler_unparse_mixed_02(): Unit = {
    val xmlReader: XMLReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    val bao = new ByteArrayOutputStream()
    val wbc = java.nio.channels.Channels.newChannel(bao)
    val unparseContentHandler = dp.newContentHandlerInstance(wbc)
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    val mixedInfoset = <list xmlns="http://example.com"><w>9</w><w>1</w><w>0</w>mixed</list>
    val bai = new ByteArrayInputStream(mixedInfoset.toString.getBytes)
    val e = intercept[SAXException] {
      xmlReader.parse(new InputSource(bai))
    }
    val m = e.getMessage()
    assertTrue(m.contains("Mixed content"))
    assertTrue(m.contains("prior to end"))
    assertTrue(m.contains("{http://example.com}list"))
  }

  @Test def testDaffodilUnhandledSAXException_creation_bothMessageAndCause(): Unit = {
    val message = "Error Message"
    val expectedException = new IllegalArgumentException("Illegal Argument Message")
    val actualException = new DaffodilUnhandledSAXException(message, expectedException)
    assertEquals(message, actualException.getMessage)
    assertEquals(expectedException, actualException.getCause)
  }

  @Test def testDaffodilUnhandledSAXException_creation_onlyMessage(): Unit = {
    val message = "Error Message"
    val actualException = new DaffodilUnhandledSAXException(message)
    assertEquals(message, actualException.getMessage)
    assertNull(actualException.getCause)
  }

  @Test def testDaffodilUnhandledSAXException_creation_onlyCause(): Unit = {
    val expectedException = new IllegalArgumentException("Illegal Argument Message")
    val actualException = new DaffodilUnhandledSAXException(expectedException)
    // when the detailMessage is null as is the case when no message is passed in,
    // getMessage returns the detailMessage from the embedded exception
    assertEquals(expectedException.getMessage, actualException.getMessage)
    assertEquals(expectedException, actualException.getCause)
  }
  @Test def testDaffodilUnhandledSAXException_creation_onlyCauseNoCauseMessage(): Unit = {
    val expectedException = new IllegalArgumentException()
    val actualException = new DaffodilUnhandledSAXException(expectedException)
    // when the detailMessage is null as is the case when no message is passed in,
    // getMessage returns the detailMessage from the embedded exception
    assertNull(actualException.getMessage)
    assertEquals(expectedException, actualException.getCause)
  }

}
