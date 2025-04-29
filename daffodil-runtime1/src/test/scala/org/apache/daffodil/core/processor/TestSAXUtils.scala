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

import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.nio.file.Paths
import scala.xml.Elem

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.DaffodilXMLLoader
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.processors.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.ParseResult

import org.jdom2.input.sax.BuilderErrorHandler
import org.junit.Assert.fail
import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.Locator

object TestSAXUtils {
  lazy val testSchema1: Elem = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"/>,
    <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" maxOccurs="unbounded"/>
        </xs:sequence>
      </xs:complexType>
  )
  lazy val dp: DataProcessor = testDataProcessor(testSchema1)
  lazy val expectedInfoset: Elem =
    <list xmlns="http://example.com"><w>9</w><w>1</w><w>0</w></list>
  lazy val testInfosetString: String = expectedInfoset.toString()
  lazy val testData: String = "910"

  lazy val loader = new DaffodilXMLLoader()

  lazy val qualifiedWithNestedSchemasFilePath =
    "/test/example_nested_namespaces_qualified.dfdl.xsd"
  lazy val qualifiedWithNestedSchemasFile =
    Misc.getRequiredResource(qualifiedWithNestedSchemasFilePath)
  lazy val qualifiedWithNestedSchemasElem: Elem =
    loader
      .load(
        URISchemaSource(
          Paths.get(qualifiedWithNestedSchemasFilePath).toFile,
          qualifiedWithNestedSchemasFile
        ),
        None
      )
      .asInstanceOf[Elem]
  lazy val dpQualifiedWithNestedSchemas: DataProcessor = testDataProcessor(
    qualifiedWithNestedSchemasElem
  )
  lazy val qualifiedWithNestedSchemasExpectedInfoset: Elem = {
    <b02:seq xmlns:xsi={
      XMLUtils.XSI_NAMESPACE
    } xmlns:b02="http://b02.com" xmlns:a02="http://a02.com">
  <b02:seq2>
    <a02:inty>3</a02:inty>
  </b02:seq2>
  <b02:seq2>
    <b02:inty>4</b02:inty>
  </b02:seq2>
  <b02:seq2>
    <a02:inty>2</a02:inty>
  </b02:seq2>
  <b02:seq2>
    <a02:intx xsi:nil="true"/>
  </b02:seq2>
  <b02:seq2>
    <b02:inty>1</b02:inty>
  </b02:seq2>
  <b02:seq2>
    <b02:inty>44</b02:inty>
  </b02:seq2>
  <b02:seq2>
    <a02:intx xsi:nil="true"/>
  </b02:seq2>
  <b02:seq2>
    <b02:inty>643</b02:inty>
  </b02:seq2>
  <b02:seq2>
    <a02:inty>3</a02:inty>
  </b02:seq2>
  <b02:seq2>
    <a02:intx xsi:nil="true"/>
  </b02:seq2>
  <b02:seq2>
    <a02:inty>5</a02:inty>
  </b02:seq2>
  <b02:seq2>
    <b02:inty>1</b02:inty>
  </b02:seq2>
</b02:seq>
  }
  lazy val qualifiedWithNestedSchemasExpectedString: String =
    qualifiedWithNestedSchemasExpectedInfoset.toString()
  lazy val qualifiedWithNestedSchemasData: String = "-3.*4.-2.^.*1.*44.^.*643.-3.^.-5.*1"

  lazy val nillableElementExpectedInfoset: Elem = {
    <b02:seq xmlns:xsi={
      XMLUtils.XSI_NAMESPACE
    } xmlns:b02="http://b02.com" xmlns:a02="http://a02.com">
      <b02:seq2>
        <a02:intx xsi:nil="true"/>
      </b02:seq2>
      <b02:seq2>
        <a02:inty>3</a02:inty>
      </b02:seq2>
      <b02:seq2>
        <b02:inty>4</b02:inty>
      </b02:seq2>
      <b02:seq2>
        <a02:intx>7</a02:intx>
      </b02:seq2>
    </b02:seq>
  }
  lazy val nillableElementExpectedString: String = nillableElementExpectedInfoset.toString()
  lazy val nillableElementData: String = "^.-3.*4.7"

  private val unqualifiedNoNamespacesFilePath = "/test/example_no_targetnamespace.dfdl.xsd"
  lazy val unqualifiedNoNamespacesFile =
    Misc.getRequiredResource(unqualifiedNoNamespacesFilePath)
  lazy val unqualifiedNoNamespacesElem: Elem =
    loader
      .load(
        URISchemaSource(
          Paths.get(unqualifiedNoNamespacesFilePath).toFile,
          unqualifiedNoNamespacesFile
        ),
        None
      )
      .asInstanceOf[Elem]

  /**
   * For an unqualified schemas with no targetnamespace and no default namespace, which means its
   * elements are not in any namespace and there are no prefixes
   * That schema references an element in a qualified namespace with a default
   * namespace, which means it will have no prefix (default) but be in its default namespace
   */
  lazy val dpUnqualifiedNoNamespaces: DataProcessor = testDataProcessor(
    unqualifiedNoNamespacesElem
  )
  lazy val unqualifiedNoNamespacesExpectedInfoset: Elem = {
    <x xmlns:xsi={XMLUtils.XSI_NAMESPACE}>
  <y>world</y>
  <y>no</y>
  <y xsi:nil="true"/>
  <y>tea</y>
</x>
  }
  lazy val unqualifiedNoNamespacesExpectedString: String =
    unqualifiedNoNamespacesExpectedInfoset.toString()
  lazy val unqualifiedNoNamespacesData: String = "world.no.^.tea"

  lazy val unqualifiedWithNestedQualifiedFilePath =
    "/test/example_nested_namespaces_unqualified.dfdl.xsd"
  lazy val unqualifiedWithNestedQualifiedFile =
    Misc.getRequiredResource(unqualifiedWithNestedQualifiedFilePath)
  lazy val unqualifiedWithNestedQualifiedElem: Elem =
    loader
      .load(
        URISchemaSource(
          Paths.get(unqualifiedWithNestedQualifiedFilePath).toFile,
          unqualifiedWithNestedQualifiedFile
        ),
        None
      )
      .asInstanceOf[Elem]

  /**
   * For an unqualified schemas with a targetnamespace and no default namespace, which means its
   * elements are in that targetnamespace, and only global elements need a prefix (unqualified).
   * That schema references an element in a qualified namespace with a default
   * namespace, which means it will have no prefix (default) but be in its default namespace
   */
  lazy val dpUnqualifiedWithNestedQualified: DataProcessor = testDataProcessor(
    unqualifiedWithNestedQualifiedElem
  )
  lazy val unqualifiedWithNestedQualifiedExpectedInfoset: Elem = {
    <b02:a xmlns:xsi={
      XMLUtils.XSI_NAMESPACE
    } xmlns:b02="http://b02.com" xmlns:c02="http://c02.com">
  <b>
    <c xmlns="http://c02.com">
      <d>hello</d>
    </c>
  </b>
  <b>
    <c xmlns="http://c02.com">
      <d xsi:nil="true"/>
    </c>
  </b>
  <b>
    <c xmlns="http://c02.com">
      <d>bye</d>
    </c>
  </b>
</b02:a>
  }
  lazy val unqualifiedWithNestedQualifiedExpectedString: String =
    unqualifiedWithNestedQualifiedExpectedInfoset.toString()
  lazy val unqualifiedWithNestedQualifiedData: String = "hello.^.bye"

  private val qualifiedWithDefaultNamespaceFilePath =
    "/test/example_c02_targetnamespace_qualified.dfdl.xsd"
  lazy val qualifiedWithDefaultNamespaceFile =
    Misc.getRequiredResource(qualifiedWithDefaultNamespaceFilePath)
  lazy val qualifiedWithDefaultNamespaceElem: Elem =
    loader
      .load(
        URISchemaSource(
          Paths.get(qualifiedWithDefaultNamespaceFilePath).toFile,
          qualifiedWithDefaultNamespaceFile
        ),
        None
      )
      .asInstanceOf[Elem]
  lazy val dpQualifiedWithDefaultNamespaceSchemas: DataProcessor = testDataProcessor(
    qualifiedWithDefaultNamespaceElem
  )
  lazy val qualifiedWithDefaultNamespaceExpectedInfoset: Elem = {
    <c xmlns="http://c02.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <d>hello</d>
</c>
  }
  lazy val qualifiedWithDefaultNamespaceExpectedString: String =
    qualifiedWithDefaultNamespaceExpectedInfoset.toString()
  lazy val qualifiedWithDefaultNamespaceData: String = "hello"

  private val qualifiedWithDefaultAndNestedSchemasFilePath =
    "/test/example_nested_namespaces_qualified_with_default.dfdl.xsd"
  lazy val qualifiedWithDefaultAndNestedSchemasFile =
    Misc.getRequiredResource(qualifiedWithDefaultAndNestedSchemasFilePath)
  lazy val qualifiedWithDefaultAndNestedSchemasElem: Elem =
    loader
      .load(
        URISchemaSource(
          Paths.get(qualifiedWithDefaultAndNestedSchemasFilePath).toFile,
          qualifiedWithDefaultAndNestedSchemasFile
        ),
        None
      )
      .asInstanceOf[Elem]
  lazy val dpQualifiedWithDefaultAndNestedSchemas: DataProcessor = testDataProcessor(
    qualifiedWithDefaultAndNestedSchemasElem
  )
  lazy val qualifiedWithDefaultAndNestedSchemasExpectedInfoset: Elem = {
    <a  xmlns="http://b02.com" xmlns:xsi={XMLUtils.XSI_NAMESPACE} xmlns:c02="http://c02.com">
  <b>
    <c xmlns="http://c02.com">
      <d>test</d>
    </c>
  </b>
  <b>
    <c xmlns="http://c02.com">
      <d>ting</d>
    </c>
  </b>
</a>
  }
  lazy val qualifiedWithDefaultAndNestedSchemasExpectedString: String =
    qualifiedWithDefaultAndNestedSchemasExpectedInfoset.toString()
  lazy val qualifiedWithDefaultAndNestedSchemasData: String = "test.ting"

  def testDataProcessor(
    testSchema: scala.xml.Elem,
    tunablesArg: Map[String, String] = Map.empty
  ): DataProcessor = {
    val schemaCompiler = Compiler().withTunables(tunablesArg)

    val pf = schemaCompiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("pf compile errors: " + msgs)
    }
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) {
      val msgs = dp.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("dp compile errors: " + msgs)
    }
    dp
  }

  def saxParseWithFeatures(
    dp: DataProcessor,
    data: String,
    namespaces: Boolean,
    namespacePrefixes: Boolean
  ): (ParseResult, scala.xml.Elem) = {
    val (
      xmlReader: DFDL.DaffodilParseXMLReader,
      baos: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupSAXParserTest(dp, data, pretty = true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, namespaces)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, namespacePrefixes)
    xmlReader.parse(inArray)
    val pr =
      xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[ParseResult]
    val actualInfoset = scala.xml.XML.loadString(baos.toString)
    (pr, actualInfoset)
  }

  def saxTraceParseWithFeatures(
    dp: DataProcessor,
    data: String,
    namespaces: Boolean,
    namespacePrefixes: Boolean
  ): ByteArrayOutputStream = {
    val (
      xmlReader: DFDL.DaffodilParseXMLReader,
      baos: ByteArrayOutputStream,
      inArray: Array[Byte]
    ) = setupTraceSAXParserTest(dp, data, pretty = true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, namespaces)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, namespacePrefixes)
    xmlReader.parse(inArray)
    val traceContentHandler = xmlReader.getContentHandler.asInstanceOf[TestContentHandler]
    traceContentHandler.fini()
    baos
  }

  def setupSAXParserTest(
    dp: DFDL.DataProcessor,
    data: String,
    pretty: Boolean = false
  ): (DFDL.DaffodilParseXMLReader, ByteArrayOutputStream, Array[Byte]) = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseContentHandler = new DaffodilParseOutputStreamContentHandler(baos, pretty = pretty)
    val eh = new BuilderErrorHandler
    xmlReader.setErrorHandler(eh)
    xmlReader.setContentHandler(parseContentHandler)
    val inArray = data.getBytes()
    (xmlReader, baos, inArray)
  }

  def setupTraceSAXParserTest(
    dp: DFDL.DataProcessor,
    data: String,
    pretty: Boolean = false
  ): (DFDL.DaffodilParseXMLReader, ByteArrayOutputStream, Array[Byte]) = {
    val xmlReader = dp.newXMLReaderInstance
    val baos = new ByteArrayOutputStream()
    val parseContentHandler = new TestContentHandler(baos)
    val eh = new BuilderErrorHandler
    xmlReader.setErrorHandler(eh)
    xmlReader.setContentHandler(parseContentHandler)
    val inArray = data.getBytes()
    (xmlReader, baos, inArray)
  }

}

/**
 * Test ContentHandler class that is used to test that XMLReader provides the expected input
 * to ContentHandler classes
 *
 * @param out output Stream of choice that the trace will be written to
 */
class TestContentHandler(out: OutputStream) extends ContentHandler {
  val sb: StringBuilder = new StringBuilder
  private val writer = new OutputStreamWriter(out)

  /**
   * platform specific newline
   */
  private val newLine = System.lineSeparator()

  override def startDocument(): Unit = writer.write(s"startDocument$newLine")

  override def endDocument(): Unit = writer.write(s"endDocument$newLine")

  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    writer.write(s"startPrefixMapping($prefix, $uri)$newLine")
  }

  override def endPrefixMapping(prefix: String): Unit = {
    writer.write(s"endPrefixMapping($prefix)$newLine")
  }

  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    atts: Attributes
  ): Unit = {
    writer.write(s"startElement($uri, $localName, $qName, ${attributesToString(atts)})$newLine")
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    writer.write(s"endElement($uri, $localName, $qName)$newLine")
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    writer.write(s"character(${ch.mkString("Array(", ",", ")")}, $start, $length)$newLine")
  }

  override def setDocumentLocator(locator: Locator): Unit = {
    // do nothing
  }

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    // do nothing
  }

  override def processingInstruction(target: String, data: String): Unit = {
    // do nothing
  }

  override def skippedEntity(name: String): Unit = {
    // do nothing
  }

  def attributesToString(atts: Attributes): String = {
    sb.setLength(0)
    val len = atts.getLength
    sb ++= "Attributes("
    for (i <- 0 until len) {
      sb ++= s"(${atts.getURI(i)},${atts.getLocalName(i)},${atts.getQName(i)},${atts.getValue(i)})"
    }
    sb ++= ")"
    sb.toString()
  }

  def fini(): Unit = {
    writer.flush()
  }

}
