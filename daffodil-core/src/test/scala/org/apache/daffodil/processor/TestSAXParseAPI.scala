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
import java.io.IOException

import scala.xml.SAXParseException

import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.infoset.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.processors.ParseResult
import org.apache.daffodil.xml.XMLUtils
import org.jdom2.input.sax.BuilderErrorHandler
import org.jdom2.input.sax.SAXHandler
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.InputSource
import org.xml.sax.SAXNotRecognizedException
import org.xml.sax.SAXNotSupportedException

class TestSAXParseAPI {
  import TestSAXParseUnparseAPI._

  @Test def testDaffodilParseXMLReader_setFeatureUnsupported(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val snr = intercept[SAXNotRecognizedException] {
      xmlReader.setFeature("http://xml.org/sax/features/validation", true)
    }
    assertTrue(snr.getMessage.contains("Feature unsupported"))
    assertTrue(snr.getMessage.contains("Supported features are:"))
  }

  @Test def testDaffodilParseXMLReader_get_setFeature(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val feature = "http://xml.org/sax/features/namespace-prefixes"
    val origValue = xmlReader.getFeature(feature)
    assertFalse(origValue)
    xmlReader.setFeature(feature, true)
    val newValue = xmlReader.getFeature(feature)
    assertTrue(newValue)
  }

  @Test def testDaffodilParseXMLReader_setProperty_unsupported(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val property: AnyRef = "Hello"
    val snr = intercept[SAXNotRecognizedException] {
      xmlReader.setProperty("http://xml.org/sax/properties/xml-string", property)
    }
    assertTrue(snr.getMessage.contains("Property unsupported"))
  }

  @Test def testDaffodilParseXMLReader_setProperty_badValue(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val property: String = XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY
    val propertyVal: AnyRef = "/tmp/i/am/a/directory"
    val sns = intercept[SAXNotSupportedException](
      xmlReader.setProperty(property, propertyVal)
    )
    assertTrue(sns.getMessage.contains("Unsupported value for property"))
  }

  @Test def testDaffodilParseXMLReader_get_setProperty(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val property: String = XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX
    val propertyVal: AnyRef ="testing-blobs"
    val origValue = xmlReader.getProperty(property)
    assertNotEquals(propertyVal, origValue)
    xmlReader.setProperty(property, propertyVal)
    val newValue = xmlReader.getProperty(property)
    assertEquals(propertyVal, newValue.asInstanceOf[String])
  }

  @Test def testDaffodilParseXMLReader_get_setContentHandler(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val parseContentHandler = new SAXHandler()
    val origValue = xmlReader.getContentHandler
    assertNull(origValue)
    xmlReader.setContentHandler(parseContentHandler)
    val newValue = xmlReader.getContentHandler
    assertTrue(newValue.isInstanceOf[SAXHandler])
  }

  @Test def testDaffodilParseXMLReader_get_setErrorHandler(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val eh = new BuilderErrorHandler()
    val origValue = xmlReader.getErrorHandler
    assertNull(origValue)
    xmlReader.setErrorHandler(eh)
    val newValue = xmlReader.getErrorHandler
    assertTrue(newValue.isInstanceOf[BuilderErrorHandler])
  }

  @Test def testDaffodilParseXMLReader_parse_inputSource_no_backing_stream(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val input = new InputSource()
    val ioe = intercept[IOException](
      xmlReader.parse(input)
    )
    assertTrue(ioe.getMessage.contains("InputSource must be backed by InputStream"))
  }

  @Test def testDaffodilParseXMLReader_parse_inputSource_with_backing_stream(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baos)
    xmlReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = testData.getBytes()
    val bais = new ByteArrayInputStream(inArray)
    val input = new InputSource(bais)
    xmlReader.parse(input)
    val pr = xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, scala.xml.XML.loadString(baos.toString))
  }

  @Test def testDaffodilParseXMLReader_parse_inputStream(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baos)
    xmlReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = testData.getBytes()
    val bais = new ByteArrayInputStream(inArray)
    xmlReader.parse(bais)
    val pr = xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, scala.xml.XML.loadString(baos.toString))
  }

  @Test def testDaffodilParseXMLReader_parse_byteArray(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baos)
    xmlReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = testData.getBytes()
    xmlReader.parse(inArray)
    val pr = xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, scala.xml.XML.loadString(baos.toString))
  }

  @Test def testDaffodilParseXMLReader_parse_errorHandler_empty_byteArray(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baos)
    xmlReader.setContentHandler(parseOutputStreamContentHandler)
    val eh = new BuilderErrorHandler
    xmlReader.setErrorHandler(eh)
    val inArray = "".getBytes()
    val spe = intercept[SAXParseException](
      xmlReader.parse(inArray)
    )
    val pr = xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(pr.isError)
    assertTrue(spe.getMessage.contains("Insufficient bits in data"))
  }
}
