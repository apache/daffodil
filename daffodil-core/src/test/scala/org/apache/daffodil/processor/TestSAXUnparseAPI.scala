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

import javax.xml.parsers.SAXParserFactory
import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.xml.XMLUtils
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.InputSource
import org.xml.sax.XMLReader

class TestSAXUnparseAPI {
  import TestSAXParseUnparseAPI._

  @Test def testUnparseContentHandler_unparse(): Unit = {
    val xmlReader: XMLReader = SAXParserFactory.newInstance.newSAXParser.getXMLReader
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
      testDataProcessor(testSchema, Map("saxUnparseEventBatchSize" -> "0"))
     }
     val eMsg = e.getMessage
     assertTrue(eMsg.contains("saxUnparseEventBatchSize"))
     assertTrue(eMsg.contains("0"))
  }

  @Test def testUnparseContentHandler_unparse_namespace_feature(): Unit = {
    val xmlReader: XMLReader = SAXParserFactory.newInstance.newSAXParser.getXMLReader
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

  @Test def testUnparseContentHandler_unparse_namespace_prefix_feature(): Unit = {
    val xmlReader: XMLReader = SAXParserFactory.newInstance.newSAXParser.getXMLReader
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
}
