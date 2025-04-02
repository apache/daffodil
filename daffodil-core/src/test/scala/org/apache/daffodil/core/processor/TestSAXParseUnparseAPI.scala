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

import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.xml.DaffodilSAXParserFactory
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.runtime1.processors.ParseResult

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.InputSource

class TestSAXParseUnparseAPI {
  import TestSAXUtils._

  /**
   * Tests the case where we use SAX to parse data and SAX unparse to unparse the parsed data
   */
  @Test def test_DaffodilParseXMLReader_parse_DaffodilUnparseContentHandler_unparse(): Unit = {
    val (
      parseXMLReader: DFDL.DaffodilParseXMLReader,
      baosParse: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, testData)
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val pr = parseXMLReader
      .getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT)
      .asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, scala.xml.XML.loadString(baosParse.toString))
    val unparseXMLReader = DaffodilSAXParserFactory().newSAXParser().getXMLReader
    unparseXMLReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    val baosUnparse = new ByteArrayOutputStream()
    val wbcUnparse = java.nio.channels.Channels.newChannel(baosUnparse)
    val unparseContentHandler = dp.newContentHandlerInstance(wbcUnparse)
    unparseXMLReader.setContentHandler(unparseContentHandler)
    val baisUnparse = new ByteArrayInputStream(baosParse.toByteArray)
    val inputSourceUnparse = new InputSource(baisUnparse)
    unparseXMLReader.parse(inputSourceUnparse)
    val ur = unparseContentHandler.getUnparseResult
    val unparsedData = baosUnparse.toString
    assertTrue(!ur.isError)
    assertEquals(testData, unparsedData)
  }

  /**
   * Tests the case where we use StAX to parse data and SAX to unparse the parse data
   */
  @Test def test_DataProcessor_parse_DaffodilUnparseContentHandler_unparse(): Unit = {
    val inArray = testData.getBytes()
    val isdis = InputSourceDataInputStream(inArray)
    val sioo = new ScalaXMLInfosetOutputter()
    val pr = dp.parse(isdis, sioo)
    val parsedData = sioo.getResult()
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, parsedData)

    val unparseXMLReader = DaffodilSAXParserFactory().newSAXParser().getXMLReader
    unparseXMLReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    val baosUnparse = new ByteArrayOutputStream()
    val wbcUnparse = java.nio.channels.Channels.newChannel(baosUnparse)
    val unparseContentHandler = dp.newContentHandlerInstance(wbcUnparse)
    unparseXMLReader.setContentHandler(unparseContentHandler)
    val baisUnparse = new ByteArrayInputStream(parsedData.toString.getBytes)
    val inputSourceUnparse = new InputSource(baisUnparse)
    unparseXMLReader.parse(inputSourceUnparse)
    val ur = unparseContentHandler.getUnparseResult
    val unparsedData = baosUnparse.toString
    assertTrue(!ur.isError)
    assertEquals(testData, unparsedData)
  }

  /**
   * test the case where we use SAX to unparse data and SAX to parse the unparsed data
   */
  @Test def test_DaffodilUnparseContentHandler_unparse_DaffodilParseXMLReader_parse(): Unit = {
    val unparseXMLReader = DaffodilSAXParserFactory().newSAXParser().getXMLReader
    unparseXMLReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    val baosUnparse = new ByteArrayOutputStream()
    val wbcUnparse = java.nio.channels.Channels.newChannel(baosUnparse)
    val unparseContentHandler = dp.newContentHandlerInstance(wbcUnparse)
    unparseXMLReader.setContentHandler(unparseContentHandler)
    val baisUnparse = new ByteArrayInputStream(testInfosetString.getBytes)
    val inputSourceUnparse = new InputSource(baisUnparse)
    unparseXMLReader.parse(inputSourceUnparse)
    val ur = unparseContentHandler.getUnparseResult
    val unparsedData = baosUnparse.toString
    assertTrue(!ur.isError)
    assertEquals(testData, unparsedData)

    val (
      parseXMLReader: DFDL.DaffodilParseXMLReader,
      baosParse: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, baosUnparse.toString)
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val pr = parseXMLReader
      .getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT)
      .asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, scala.xml.XML.loadString(baosParse.toString))
  }

  /**
   * tests the case where we use SAX to unparse data and StAX to parse the unparsed data
   */
  @Test def test_DaffodilUnparseContentHandler_unparse_DataProcessor_parse(): Unit = {
    val unparseXMLReader = DaffodilSAXParserFactory().newSAXParser().getXMLReader
    unparseXMLReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    val baosUnparse = new ByteArrayOutputStream()
    val wbcUnparse = java.nio.channels.Channels.newChannel(baosUnparse)
    val unparseContentHandler = dp.newContentHandlerInstance(wbcUnparse)
    unparseXMLReader.setContentHandler(unparseContentHandler)
    val baisUnparse = new ByteArrayInputStream(testInfosetString.getBytes)
    val inputSourceUnparse = new InputSource(baisUnparse)
    unparseXMLReader.parse(inputSourceUnparse)
    val ur = unparseContentHandler.getUnparseResult
    val unparsedData = baosUnparse.toString
    assertTrue(!ur.isError)
    assertEquals(testData, unparsedData)

    val inArray = unparsedData.getBytes()
    val isdis = InputSourceDataInputStream(inArray)
    val sioo = new ScalaXMLInfosetOutputter()
    val pr = dp.parse(isdis, sioo)
    val parsedData = sioo.getResult()
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, parsedData)
  }

  @Test def test_DaffodilParseXMLReader_parse_DataProcessor_unparse(): Unit = {
    val (
      parseXMLReader: DFDL.DaffodilParseXMLReader,
      baosParse: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, testData)
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val parsedNode = scala.xml.XML.loadString(baosParse.toString)
    val pr = parseXMLReader
      .getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT)
      .asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, parsedNode)

    val baosUnparse = new ByteArrayOutputStream()
    val wbcUnparse = java.nio.channels.Channels.newChannel(baosUnparse)
    val sii = new ScalaXMLInfosetInputter(parsedNode)
    val ur = dp.unparse(sii, wbcUnparse)
    val unparsedData = baosUnparse.toString
    assertTrue(!ur.isError)
    assertEquals(testData, unparsedData)
  }

  @Test def test_DataProcessor_unparse_DaffodilParseXMLReader_parse(): Unit = {
    val baosUnparse = new ByteArrayOutputStream()
    val wbcUnparse = java.nio.channels.Channels.newChannel(baosUnparse)
    val sii = new ScalaXMLInfosetInputter(expectedInfoset)
    val ur = dp.unparse(sii, wbcUnparse)
    assertTrue(!ur.isError)
    assertEquals(testData, baosUnparse.toString)

    val (
      parseXMLReader: DFDL.DaffodilParseXMLReader,
      baosParse: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, baosUnparse.toString)
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val pr = parseXMLReader
      .getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT)
      .asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, scala.xml.XML.loadString(baosParse.toString))
  }

  @Test
  def test_entityRefInXML(): Unit = {
    import InfosetInputterEventType._

    val sii = new ScalaXMLInfosetInputter(<foo>&amp;</foo>)
    assertTrue(sii.hasNext())
    sii.next()
    assertEquals(StartElement, sii.getEventType())
    val stxt = sii.getSimpleText(NodeInfo.String, java.util.Collections.emptyMap())
    assertEquals("&", stxt)
  }
}
