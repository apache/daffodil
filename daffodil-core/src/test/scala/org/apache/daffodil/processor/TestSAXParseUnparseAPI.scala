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

import scala.xml.Elem

import javax.xml.parsers.SAXParserFactory
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.infoset.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.ParseResult
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.xml.XMLUtils
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test
import org.xml.sax.InputSource

object TestSAXParseUnparseAPI {
  val testSchema: Elem = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:complexType>
  )
  val testInfoset: Elem = <list xmlns="http://example.com"><w>9</w><w>1</w><w>0</w></list>
  val testInfosetString: String = testInfoset.toString()
  val testData = "910"

  lazy val dp: DataProcessor = testDataProcessor(testSchema)

  def testDataProcessor(testSchema: scala.xml.Elem, tunablesArg: Map[String, String] = Map.empty): DataProcessor = {
    val schemaCompiler = Compiler().withTunables(tunablesArg)

    val pf = schemaCompiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("pf compile errors: " + msgs)
    }
    pf.sset.root.erd.preSerialization // force evaluation of all compile-time constructs
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) {
      val msgs = dp.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("dp compile errors: " + msgs)
    }
    dp
  }
}

class TestSAXParseUnparseAPI {
  import TestSAXParseUnparseAPI._

  @Test def test_DaffodilParseXMLReader_parse_DaffodilUnparseContentHandler_unparse(): Unit = {
    val parseXMLReader = dp.newXMLReaderInstance
    val baosParse = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baosParse)
    parseXMLReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = testData.getBytes()
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val pr = parseXMLReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, scala.xml.XML.loadString(baosParse.toString))

    val unparseXMLReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader
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

  @Test def test_DataProcessor_parse_DaffodilUnparseContentHandler_unparse(): Unit = {
    val inArray = testData.getBytes()
    val isdis = InputSourceDataInputStream(inArray)
    val sioo = new ScalaXMLInfosetOutputter()
    val pr = dp.parse(isdis, sioo)
    val parsedData = sioo.getResult()
    assertTrue(!pr.isError)
    assertEquals(testInfoset, parsedData)

    val unparseXMLReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader
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

  @Test def test_DaffodilUnparseContentHandler_unparse_DaffodilParseXMLReader_parse(): Unit = {
    val unparseXMLReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader
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

    val parseXMLReader = dp.newXMLReaderInstance
    val baosParse = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baosParse)
    parseXMLReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = baosUnparse.toByteArray
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val pr = parseXMLReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, scala.xml.XML.loadString(baosParse.toString))
  }

  @Test def test_DaffodilUnparseContentHandler_unparse_DataProcessor_parse(): Unit = {
    val unparseXMLReader = SAXParserFactory.newInstance().newSAXParser().getXMLReader
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
    assertEquals(testInfoset, parsedData)
  }

  @Test def test_DaffodilParseXMLReader_parse_DataProcessor_unparse(): Unit = {
    val parseXMLReader = dp.newXMLReaderInstance
    val baosParse = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baosParse)
    parseXMLReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = testData.getBytes()
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val parsedNode = scala.xml.XML.loadString(baosParse.toString)
    val pr = parseXMLReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, parsedNode)

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
    val sii = new ScalaXMLInfosetInputter(testInfoset)
    val ur = dp.unparse(sii, wbcUnparse)
    assertTrue(!ur.isError)
    assertEquals(testData, baosUnparse.toString)

    val parseXMLReader = dp.newXMLReaderInstance
    val baosParse = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baosParse)
    parseXMLReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = baosUnparse.toByteArray
    val baisParse = new ByteArrayInputStream(inArray)
    val inputSourceParse = new InputSource(baisParse)
    parseXMLReader.parse(inputSourceParse)
    val pr = parseXMLReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(testInfoset, scala.xml.XML.loadString(baosParse.toString))
  }
}
