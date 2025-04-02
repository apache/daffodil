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
import java.io.IOException
import scala.xml.SAXParseException

import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.processors.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.runtime1.processors.ParseResult

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
  import TestSAXUtils._

  /**
   * tests the case where we attempt to set an unsupported feature
   */
  @Test def testDaffodilParseXMLReader_setFeature_unsupported(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val snr = intercept[SAXNotRecognizedException] {
      xmlReader.setFeature("http://xml.org/sax/features/validation", true)
    }
    assertTrue(snr.getMessage.contains("Feature unsupported"))
    assertTrue(snr.getMessage.contains("Supported features are:"))
  }

  /**
   * tests the case where we attempt to get an unsupported feature
   */
  @Test def testDaffodilParseXMLReader_getFeature_unsupported(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val snr = intercept[SAXNotRecognizedException] {
      xmlReader.getFeature("http://xml.org/sax/features/validation")
    }
    assertTrue(snr.getMessage.contains("Feature unsupported"))
    assertTrue(snr.getMessage.contains("Supported features are:"))
  }

  /**
   * tests that we can set and get a supported feature
   */
  @Test def testDaffodilParseXMLReader_get_setFeature(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val feature = XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE
    val origValue = xmlReader.getFeature(feature)
    assertFalse(origValue)
    xmlReader.setFeature(feature, true)
    val newValue = xmlReader.getFeature(feature)
    assertTrue(newValue)
  }

  /*
   * tests the case where both supported features are set to false, as it is an illegal state
   */
  @Test def testDaffodilParseXMLReader_supportedFeaturesNotFalse(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseOutputStreamContentHandler = new DaffodilParseOutputStreamContentHandler(baos)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, false)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, false)
    xmlReader.setContentHandler(parseOutputStreamContentHandler)
    val inArray = testData.getBytes()
    val snr = intercept[org.xml.sax.SAXException] {
      xmlReader.parse(inArray)
    }
    assertTrue(snr.getMessage.contains("Namespaces and NamespacePrefixes"))
    assertTrue(snr.getMessage.contains("cannot both be false"))
  }

  /**
   * tests the case where we attempt to set an unsupported property
   */
  @Test def testDaffodilParseXMLReader_setProperty_unsupported(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val property: AnyRef = "Hello"
    val snr = intercept[SAXNotRecognizedException] {
      xmlReader.setProperty("http://xml.org/sax/properties/xml-string", property)
    }
    assertTrue(snr.getMessage.contains("Property unsupported"))
  }

  /**
   * tests the case where we attempt to get an unsupported property
   */
  @Test def testDaffodilParseXMLReader_getProperty_unsupported(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val snr = intercept[SAXNotRecognizedException] {
      xmlReader.getProperty("http://xml.org/sax/properties/xml-string")
    }
    assertTrue(snr.getMessage.contains("Property unsupported"))
  }

  /**
   * tests the case where we attempt to set an invalid value for a supported property
   */
  @Test def testDaffodilParseXMLReader_setProperty_badValue(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val property: String = XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY
    val propertyVal: AnyRef = "/tmp/i/am/a/directory"
    val sns = intercept[SAXNotSupportedException](
      xmlReader.setProperty(property, propertyVal)
    )
    assertTrue(sns.getMessage.contains("Unsupported value for property"))
  }

  /**
   * tests that we can set and get a supported property
   */
  @Test def testDaffodilParseXMLReader_get_setProperty(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val property: String = XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX
    val propertyVal: AnyRef = "testing-blobs"
    val origValue = xmlReader.getProperty(property)
    assertNotEquals(propertyVal, origValue)
    xmlReader.setProperty(property, propertyVal)
    val newValue = xmlReader.getProperty(property)
    assertEquals(propertyVal, newValue.asInstanceOf[String])
  }

  /**
   * test that we can get and set a contentHandler
   */
  @Test def testDaffodilParseXMLReader_get_setContentHandler(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val parseContentHandler = new SAXHandler()
    val origValue = xmlReader.getContentHandler
    assertNull(origValue)
    xmlReader.setContentHandler(parseContentHandler)
    val newValue = xmlReader.getContentHandler
    assertTrue(newValue.isInstanceOf[SAXHandler])
  }

  /**
   * test that we can get and set a errorHandler
   */
  @Test def testDaffodilParseXMLReader_get_setErrorHandler(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val eh = new BuilderErrorHandler()
    val origValue = xmlReader.getErrorHandler
    assertNull(origValue)
    xmlReader.setErrorHandler(eh)
    val newValue = xmlReader.getErrorHandler
    assertTrue(newValue.isInstanceOf[BuilderErrorHandler])
  }

  /**
   * tests that an exception occurs if we try to parse an inputSource with no backing inputStream
   */
  @Test def testDaffodilParseXMLReader_parse_inputSource_no_backing_stream(): Unit = {
    val xmlReader = dp.newXMLReaderInstance
    val input = new InputSource()
    val ioe = intercept[IOException](
      xmlReader.parse(input)
    )
    assertTrue(ioe.getMessage.contains("InputSource must be backed by InputStream"))
  }

  /**
   * tests that we can parse using an inputSource with a backing inputStream
   */
  @Test def testDaffodilParseXMLReader_parse_inputSource_with_backing_stream(): Unit = {
    val (
      xmlReader: DFDL.DaffodilParseXMLReader,
      baos: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, testData)
    val bais = new ByteArrayInputStream(inArray)
    val input = new InputSource(bais)
    xmlReader.parse(input)
    val pr =
      xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    val actualInfoset = scala.xml.XML.loadString(baos.toString)
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, actualInfoset)
  }

  /**
   * tests that we can parse using an inputStream
   */
  @Test def testDaffodilParseXMLReader_parse_inputStream(): Unit = {
    val (
      xmlReader: DFDL.DaffodilParseXMLReader,
      baos: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, testData)
    val bais = new ByteArrayInputStream(inArray)
    xmlReader.parse(bais)
    val pr =
      xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    val actualInfoset = scala.xml.XML.loadString(baos.toString)
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, actualInfoset)
  }

  /**
   * tests that we can parse using a byte array
   */
  @Test def testDaffodilParseXMLReader_parse_byteArray(): Unit = {
    val (
      xmlReader: DFDL.DaffodilParseXMLReader,
      baos: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, testData)
    xmlReader.parse(inArray)
    val actualInfoset = scala.xml.XML.loadString(baos.toString)
    val pr =
      xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(!pr.isError)
    assertEquals(expectedInfoset, actualInfoset)
  }

  /**
   * tests that the error handler is populated if we try to parse an empty input
   */
  @Test def testDaffodilParseXMLReader_parse_errorHandler_empty_byteArray(): Unit = {
    val (xmlReader: DFDL.DaffodilParseXMLReader, _, inArray: Array[Byte]) =
      setupSAXParserTest(dp, "")
    val eh = new BuilderErrorHandler
    xmlReader.setErrorHandler(eh)
    val spe = intercept[SAXParseException](
      xmlReader.parse(inArray)
    )
    val pr =
      xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    assertTrue(pr.isError)
    assertTrue(spe.getMessage.contains("Insufficient bits in data"))
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true, for a data that generates an infoset
   * with nested elements from different namespaces, including nil elements
   */
  @Test def testDaffodilParseXMLReader_parse_features_prefixes_only1(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithNestedSchemas,
      qualifiedWithNestedSchemasData,
      namespaces = false,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithNestedSchemasExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true, qnames are populated for elements,
   * but there not uris and localnames; attributes contain prefix mappings where the qname has the
   * xmlns* and the value contains the associated uri. Regular elements are also provided by attributes
   */
  @Test def testDaffodilParseXMLReader_parse_features_prefixes_only2(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpUnqualifiedNoNamespaces,
      unqualifiedNoNamespacesData,
      namespaces = false,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(unqualifiedNoNamespacesExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true., for data that generates an infoset
   * with nested elements
   */
  @Test def testDaffodilParseXMLReader_parse_features_prefixes_only3(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpUnqualifiedWithNestedQualified,
      unqualifiedWithNestedQualifiedData,
      namespaces = false,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(unqualifiedWithNestedQualifiedExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_prefixes_only4(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithDefaultNamespaceSchemas,
      qualifiedWithDefaultNamespaceData,
      namespaces = false,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithDefaultNamespaceExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_prefixes_only5(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithDefaultAndNestedSchemas,
      qualifiedWithDefaultAndNestedSchemasData,
      namespaces = false,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithDefaultAndNestedSchemasExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix false is set to false.
   */
  @Test def testDaffodilParseXMLReader_parse_features_namespace_only1(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithNestedSchemas,
      qualifiedWithNestedSchemasData,
      namespaces = true,
      namespacePrefixes = false
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithNestedSchemasExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix is set to false.
   */
  @Test def testDaffodilParseXMLReader_parse_features_namespace_only2(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpUnqualifiedNoNamespaces,
      unqualifiedNoNamespacesData,
      namespaces = true,
      namespacePrefixes = false
    )
    assertTrue(!pr.isError)
    assertEquals(unqualifiedNoNamespacesExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix false is set to false.
   */
  @Test def testDaffodilParseXMLReader_parse_features_namespace_only3(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpUnqualifiedWithNestedQualified,
      unqualifiedWithNestedQualifiedData,
      namespaces = true,
      namespacePrefixes = false
    )
    assertTrue(!pr.isError)
    assertEquals(unqualifiedWithNestedQualifiedExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix false is set to false.
   */
  @Test def testDaffodilParseXMLReader_parse_features_namespace_only4(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithDefaultNamespaceSchemas,
      qualifiedWithDefaultNamespaceData,
      namespaces = true,
      namespacePrefixes = false
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithDefaultNamespaceExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix false is set to false.
   */
  @Test def testDaffodilParseXMLReader_parse_features_namespace_only5(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithDefaultAndNestedSchemas,
      qualifiedWithDefaultAndNestedSchemasData,
      namespaces = true,
      namespacePrefixes = false
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithDefaultAndNestedSchemasExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces and the namespace
   * prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_both1(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithNestedSchemas,
      qualifiedWithNestedSchemasData,
      namespaces = true,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithNestedSchemasExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces and the namespace
   * prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_both2(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpUnqualifiedNoNamespaces,
      unqualifiedNoNamespacesData,
      namespaces = true,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(unqualifiedNoNamespacesExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces and the namespace
   * prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_both3(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpUnqualifiedWithNestedQualified,
      unqualifiedWithNestedQualifiedData,
      namespaces = true,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(unqualifiedWithNestedQualifiedExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces and the namespace
   * prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_both4(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithDefaultNamespaceSchemas,
      qualifiedWithDefaultNamespaceData,
      namespaces = true,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithDefaultNamespaceExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces and the namespace
   * prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_parse_features_both5(): Unit = {
    val (pr, actualInfoset) = saxParseWithFeatures(
      dpQualifiedWithDefaultAndNestedSchemas,
      qualifiedWithDefaultAndNestedSchemasData,
      namespaces = true,
      namespacePrefixes = true
    )
    assertTrue(!pr.isError)
    assertEquals(qualifiedWithDefaultAndNestedSchemasExpectedInfoset, actualInfoset)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix feature is set to false.
   */
  @Test def testDaffodilParseXMLReader_trace_features_default(): Unit = {
    val baos = saxTraceParseWithFeatures(
      dpQualifiedWithNestedSchemas,
      nillableElementData,
      namespaces = true,
      namespacePrefixes = false
    )
    val actualOutput = baos.toString
    val xsiUri = XMLUtils.XSI_NAMESPACE
    val a02Uri = "http://a02.com"
    val b02Uri = "http://b02.com"
    val expectedOutput =
      s"""startDocument
        |startPrefixMapping(a02, $a02Uri)
        |startPrefixMapping(b02, $b02Uri)
        |startPrefixMapping(xsi, $xsiUri)
        |startElement($b02Uri, seq, , Attributes())
        |startElement($b02Uri, seq2, , Attributes())
        |startElement($a02Uri, intx, , Attributes(($xsiUri,nil,,true)))
        |endElement($a02Uri, intx, )
        |endElement($b02Uri, seq2, )
        |startElement($b02Uri, seq2, , Attributes())
        |startElement($a02Uri, inty, , Attributes())
        |character(Array(3), 0, 1)
        |endElement($a02Uri, inty, )
        |endElement($b02Uri, seq2, )
        |startElement($b02Uri, seq2, , Attributes())
        |startElement($b02Uri, inty, , Attributes())
        |character(Array(4), 0, 1)
        |endElement($b02Uri, inty, )
        |endElement($b02Uri, seq2, )
        |startElement($b02Uri, seq2, , Attributes())
        |startElement($a02Uri, intx, , Attributes())
        |character(Array(7), 0, 1)
        |endElement($a02Uri, intx, )
        |endElement($b02Uri, seq2, )
        |endElement($b02Uri, seq, )
        |endPrefixMapping(a02)
        |endPrefixMapping(b02)
        |endPrefixMapping(xsi)
        |endDocument
        |""".stripMargin
    assertEquals(expectedOutput, actualOutput)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_trace_features_namespace_and_prefixes(): Unit = {
    val baos = saxTraceParseWithFeatures(
      dpQualifiedWithNestedSchemas,
      nillableElementData,
      namespaces = true,
      namespacePrefixes = true
    )
    val actualOutput = baos.toString
    val xsiUri = XMLUtils.XSI_NAMESPACE
    val a02Uri = "http://a02.com"
    val b02Uri = "http://b02.com"
    val expectedOutput =
      s"""startDocument
         |startPrefixMapping(a02, $a02Uri)
         |startPrefixMapping(b02, $b02Uri)
         |startPrefixMapping(xsi, $xsiUri)
         |startElement($b02Uri, seq, b02:seq, Attributes((,,xmlns:a02,$a02Uri)(,,xmlns:b02,$b02Uri)(,,xmlns:xsi,$xsiUri)))
         |startElement($b02Uri, seq2, b02:seq2, Attributes())
         |startElement($a02Uri, intx, a02:intx, Attributes(($xsiUri,nil,xsi:nil,true)))
         |endElement($a02Uri, intx, a02:intx)
         |endElement($b02Uri, seq2, b02:seq2)
         |startElement($b02Uri, seq2, b02:seq2, Attributes())
         |startElement($a02Uri, inty, a02:inty, Attributes())
         |character(Array(3), 0, 1)
         |endElement($a02Uri, inty, a02:inty)
         |endElement($b02Uri, seq2, b02:seq2)
         |startElement($b02Uri, seq2, b02:seq2, Attributes())
         |startElement($b02Uri, inty, b02:inty, Attributes())
         |character(Array(4), 0, 1)
         |endElement($b02Uri, inty, b02:inty)
         |endElement($b02Uri, seq2, b02:seq2)
         |startElement($b02Uri, seq2, b02:seq2, Attributes())
         |startElement($a02Uri, intx, a02:intx, Attributes())
         |character(Array(7), 0, 1)
         |endElement($a02Uri, intx, a02:intx)
         |endElement($b02Uri, seq2, b02:seq2)
         |endElement($b02Uri, seq, b02:seq)
         |endPrefixMapping(a02)
         |endPrefixMapping(b02)
         |endPrefixMapping(xsi)
         |endDocument
         |""".stripMargin
    assertEquals(expectedOutput, actualOutput)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_trace_features_prefixes_only(): Unit = {
    val baos = saxTraceParseWithFeatures(
      dpQualifiedWithNestedSchemas,
      nillableElementData,
      namespaces = false,
      namespacePrefixes = true
    )
    val actualOutput = baos.toString
    val xsiUri = XMLUtils.XSI_NAMESPACE
    val a02Uri = "http://a02.com"
    val b02Uri = "http://b02.com"
    val expectedOutput =
      s"""startDocument
         |startElement(, , b02:seq, Attributes((,,xmlns:a02,$a02Uri)(,,xmlns:b02,$b02Uri)(,,xmlns:xsi,$xsiUri)))
         |startElement(, , b02:seq2, Attributes())
         |startElement(, , a02:intx, Attributes((,,xsi:nil,true)))
         |endElement(, , a02:intx)
         |endElement(, , b02:seq2)
         |startElement(, , b02:seq2, Attributes())
         |startElement(, , a02:inty, Attributes())
         |character(Array(3), 0, 1)
         |endElement(, , a02:inty)
         |endElement(, , b02:seq2)
         |startElement(, , b02:seq2, Attributes())
         |startElement(, , b02:inty, Attributes())
         |character(Array(4), 0, 1)
         |endElement(, , b02:inty)
         |endElement(, , b02:seq2)
         |startElement(, , b02:seq2, Attributes())
         |startElement(, , a02:intx, Attributes())
         |character(Array(7), 0, 1)
         |endElement(, , a02:intx)
         |endElement(, , b02:seq2)
         |endElement(, , b02:seq)
         |endDocument
         |""".stripMargin
    assertEquals(expectedOutput, actualOutput)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix feature is set to false.
   */
  @Test def testDaffodilParseXMLReader_trace_features_default_2(): Unit = {
    val baos = saxTraceParseWithFeatures(
      dpUnqualifiedNoNamespaces,
      unqualifiedNoNamespacesData,
      namespaces = true,
      namespacePrefixes = false
    )
    val actualOutput = baos.toString
    val xsiUri = XMLUtils.XSI_NAMESPACE
    val expectedOutput =
      s"""startDocument
         |startPrefixMapping(xsi, $xsiUri)
         |startElement(, x, , Attributes())
         |startElement(, y, , Attributes())
         |character(Array(w,o,r,l,d), 0, 5)
         |endElement(, y, )
         |startElement(, y, , Attributes())
         |character(Array(n,o), 0, 2)
         |endElement(, y, )
         |startElement(, y, , Attributes(($xsiUri,nil,,true)))
         |endElement(, y, )
         |startElement(, y, , Attributes())
         |character(Array(t,e,a), 0, 3)
         |endElement(, y, )
         |endElement(, x, )
         |endPrefixMapping(xsi)
         |endDocument
         |""".stripMargin
    assertEquals(expectedOutput, actualOutput)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to true and the namespace prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_trace_features_namespace_and_prefixes_2(): Unit = {
    val baos = saxTraceParseWithFeatures(
      dpUnqualifiedNoNamespaces,
      unqualifiedNoNamespacesData,
      namespaces = true,
      namespacePrefixes = true
    )
    val actualOutput = baos.toString
    val xsiUri = XMLUtils.XSI_NAMESPACE
    val expectedOutput =
      s"""startDocument
         |startPrefixMapping(xsi, $xsiUri)
         |startElement(, x, x, Attributes((,,xmlns:xsi,$xsiUri)))
         |startElement(, y, y, Attributes())
         |character(Array(w,o,r,l,d), 0, 5)
         |endElement(, y, y)
         |startElement(, y, y, Attributes())
         |character(Array(n,o), 0, 2)
         |endElement(, y, y)
         |startElement(, y, y, Attributes(($xsiUri,nil,xsi:nil,true)))
         |endElement(, y, y)
         |startElement(, y, y, Attributes())
         |character(Array(t,e,a), 0, 3)
         |endElement(, y, y)
         |endElement(, x, x)
         |endPrefixMapping(xsi)
         |endDocument
         |""".stripMargin
    assertEquals(expectedOutput, actualOutput)
  }

  /*
   * tests that the output from the parser is as expected, when the namespaces feature is set
   * to false and the namespace prefix feature is set to true.
   */
  @Test def testDaffodilParseXMLReader_trace_features_prefixes_only_2(): Unit = {
    val baos = saxTraceParseWithFeatures(
      dpUnqualifiedNoNamespaces,
      unqualifiedNoNamespacesData,
      namespaces = false,
      namespacePrefixes = true
    )
    val actualOutput = baos.toString
    val xsiUri = XMLUtils.XSI_NAMESPACE
    val expectedOutput =
      s"""startDocument
         |startElement(, , x, Attributes((,,xmlns:xsi,$xsiUri)))
         |startElement(, , y, Attributes())
         |character(Array(w,o,r,l,d), 0, 5)
         |endElement(, , y)
         |startElement(, , y, Attributes())
         |character(Array(n,o), 0, 2)
         |endElement(, , y)
         |startElement(, , y, Attributes((,,xsi:nil,true)))
         |endElement(, , y)
         |startElement(, , y, Attributes())
         |character(Array(t,e,a), 0, 3)
         |endElement(, , y)
         |endElement(, , x)
         |endDocument
         |""".stripMargin
    assertEquals(expectedOutput, actualOutput)
  }
}
