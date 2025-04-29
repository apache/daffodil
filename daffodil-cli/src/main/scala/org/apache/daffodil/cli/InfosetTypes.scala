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

package org.apache.daffodil.cli

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.OutputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import scala.collection.mutable.ArrayBuffer
import scala.xml.SAXParser

import org.apache.daffodil.api.infoset.{ InfosetInputter => JInfosetInputter }
import org.apache.daffodil.api.infoset.{ InfosetOutputter => JInfosetOutputter }
import org.apache.daffodil.api.{ DataProcessor => JDataProcessor }
import org.apache.daffodil.api.{ ParseResult => JParseResult }
import org.apache.daffodil.api.{ UnparseResult => JUnparseResult }
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.xml.DFDLCatalogResolver
import org.apache.daffodil.lib.xml.DaffodilSAXParserFactory
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.runtime1.infoset.JDOMInfosetInputter
import org.apache.daffodil.runtime1.infoset.JDOMInfosetOutputter
import org.apache.daffodil.runtime1.infoset.JsonInfosetInputter
import org.apache.daffodil.runtime1.infoset.JsonInfosetOutputter
import org.apache.daffodil.runtime1.infoset.NullInfosetInputter
import org.apache.daffodil.runtime1.infoset.NullInfosetOutputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.runtime1.infoset.W3CDOMInfosetInputter
import org.apache.daffodil.runtime1.infoset.W3CDOMInfosetOutputter
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetInputter
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.runtime1.processors.DaffodilParseOutputStreamContentHandler

import com.siemens.ct.exi.core.EXIFactory
import com.siemens.ct.exi.core.helpers.DefaultEXIFactory
import com.siemens.ct.exi.grammars.GrammarFactory
import com.siemens.ct.exi.main.api.sax.EXIResult
import com.siemens.ct.exi.main.api.sax.EXISource
import org.apache.commons.io.IOUtils
import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.InputSource
import org.xml.sax.Locator
import org.xml.sax.XMLReader
import org.xml.sax.helpers.AttributesImpl
import org.xml.sax.helpers.DefaultHandler

object InfosetType extends Enumeration {
  type Type = Value

  val EXI = Value("exi")
  val EXISA = Value("exisa")
  val JDOM = Value("jdom")
  val JSON = Value("json")
  val NULL = Value("null")
  val SAX = Value("sax")
  val SCALA_XML = Value("scala-xml")
  val W3CDOM = Value("w3cdom")
  val XML = Value("xml")

  /**
   * Get an InfosetHandler, with the goal of doing as much initialization/work
   * as needed prior to calling the parse() or unparse() methods to improve
   * accuracy of performance metrics
   *
   * @param infosetType the type of InfosetHandler to create
   * @param dataProcessor the dataProcessor that the InfosetHandler should use
   *   during parse/unparse operations
   * @param schemaUri only used for EXISA, to support schema aware
   *   parsing/unparsing
   * @param forPerformance only used for SAX. If true, the
   *   SAXInfosetHandler will drop all SAX events on parse, and will
   *   pre-process the infoset into an array of SAX events and replay them on
   *   unparse. If false, it directly parses and unparses to/from XML
   *   text--this allows the caller to visualize the SAX as XML, similar
   *   to how JDOM, SCALA_XML, etc can be serialized to strings
   */
  def getInfosetHandler(
    infosetType: InfosetType.Type,
    dataProcessor: JDataProcessor,
    schemaUri: Option[URI],
    forPerformance: Boolean
  ): InfosetHandler = {

    infosetType match {
      case InfosetType.EXI => EXIInfosetHandler(dataProcessor)
      case InfosetType.EXISA => EXIInfosetHandler(dataProcessor, schemaUri.get)
      case InfosetType.JDOM => JDOMInfosetHandler(dataProcessor)
      case InfosetType.JSON => JsonInfosetHandler(dataProcessor)
      case InfosetType.NULL => NULLInfosetHandler(dataProcessor)
      case InfosetType.SAX => SAXInfosetHandler(dataProcessor, forPerformance)
      case InfosetType.SCALA_XML => ScalaXMLInfosetHandler(dataProcessor)
      case InfosetType.W3CDOM => W3CDOMInfosetHandler(dataProcessor)
      case InfosetType.XML => XMLTextInfosetHandler(dataProcessor)
    }
  }
}

sealed trait InfosetHandler {

  /**
    * Parse data from the InputSourceDataInputStream with the provided
    * DataProcessor and return a new InfosetParseResult instance. Depending on
    * the provided InfosetType, may or may not write an infoset instance to the
    * OutputStream during parsing. This method will be called in a performance
    * loop, so InfosetHandler constructors should do as much preprocessing and
    * initialization as possible to save time in this method.
    *
    * Some InfosetHandlers will not write anything to the OutputStream here
    * because they just create objects, call interface functions, or create
    * nothing (e.g., JDOM, SCALA_XML, NULL). However, Daffodil's CLI wants all
    * infoset types except NULL to output an infoset during parsing, so
    * InfosetHandlers return an InfosetParseResult which can serialize the
    * infoset to a string and write the string to the OutputStream. The CLI
    * does not want to output the infoset in a performance loop, so we put such
    * serialization and output code into InfosetParseResult, not
    * InfosetHandler.
    *
    * @param input Data to be parsed into an infoset
    * @param os OutputStream to write an infoset to
    */
  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult

  /**
   * Convert an Array of Bytes to whatever form the unparse() function expects
   * to use, preferably as close to the native format for the infoset type as
   * possible
   *
   * The return value of the Array[Byte] variant must be thread safe and
   * immutable since it could potentially be shared among other threaded calls
   * to unparse()
   *
   * This function is guaranteed to be called outside of a performance loop, so
   * expensive operations to create the native infoset representation should
   * take place here instead of the unparse() function.
   */
  def dataToInfoset(bytes: Array[Byte]): AnyRef

  /**
   * Convert an InputStream to whatever form the unparse() function expects to
   * use, preferably as close to the native format for the infoset type as
   * possible
   *
   * With this InputStream variant, we can assume the caller knows that the
   * infoset represented by this InputStream will only be unparsed once and so
   * it is acceptable if the result is mutable or non-thread safe. For
   * InfosetHandlers that easily support InputStreams, it is recommended to
   * simply return the same InputStream since it avoids reading the entire
   * infoset into memory and making it possible to unparse large infosets.
   * However, for InfosetHandlers that do not accept InputStreams,
   * implementations must read in the entire InputStream and convert it to
   * whatever they expect (e.g. Scala XML Node for "scala-xml"). Supporting
   * large inputs with such infoset types is not possible.
   *
   * This function is guaranteed to be called outside of a performance loop, so
   * expensive operations to create the native infoset representation should
   * take place here instead of the unparse() function.
   */
  def dataToInfoset(stream: InputStream): AnyRef

  /**
   * Unparse an infoset to the given OutputStream. This method will be called
   * in a performance loop, so InfosetHandler constructors and/or dataToInfoset()
   * functions should perform as much preprocessing and initialization as possible
   * to save time in this method.
   *
   * @param infoset Infoset representation returned by dataToInfoset() functions
   * @param output Output channel to write the unparse data to
   */
  def unparse(infoset: AnyRef, output: DFDL.Output): JUnparseResult

  /**
   * DataProcessor to be used for parse/unparse calls
   */
  protected def dataProcessor: JDataProcessor

  /**
   * Helper function to parse data using the Daffodil InfosetOutputter API
   */
  final protected def parseWithInfosetOutputter(
    input: InputSourceDataInputStream,
    output: JInfosetOutputter
  ): JParseResult = {
    output.setBlobAttributes(Main.blobDir, null, Main.blobSuffix)
    val pr = dataProcessor.parse(input, output)
    pr
  }

  /**
   * Helper function to unparse data using the Daffodil InfosetInputter API
   */
  final protected def unparseWithInfosetInputter(
    input: JInfosetInputter,
    output: DFDL.Output
  ): JUnparseResult = {
    val ur = dataProcessor.unparse(input, output)
    ur
  }

  /**
   * Helper function to parse data using the Daffodil SAX API
   */
  final protected def parseWithSax(
    input: InputSourceDataInputStream,
    contentHandler: ContentHandler
  ): JParseResult = {
    val xmlReader = dataProcessor.newXMLReaderInstance
    // SAX_NAMESPACE_PREFIXES_FEATURE is needed to preserve nil attributes with EXI
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    xmlReader.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY, Main.blobDir)
    xmlReader.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX, Main.blobSuffix)
    xmlReader.setContentHandler(contentHandler)
    xmlReader.parse(input)
    val pr =
      xmlReader.getProperty(XMLUtils.DAFFODIL_SAX_URN_PARSERESULT).asInstanceOf[JParseResult]
    pr
  }

  /**
   * Helper function to unparse data using the Daffodil SAX API
   */
  final protected def unparseWithSax(
    xmlReader: XMLReader,
    input: InputStream,
    output: DFDL.Output
  ): JUnparseResult = {
    val contentHandler = dataProcessor.newContentHandlerInstance(output)
    xmlReader.setContentHandler(contentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    try {
      xmlReader.parse(new InputSource(input))
    } catch {
      case _: DaffodilUnparseErrorSAXException => // do nothing, unparseResult has error info
    }
    val ur = contentHandler.getUnparseResult
    ur
  }

}

/**
 * Wrapper around a ParseResult that allows for serializing the ParseResult to
 * an XML string for easy visualization for caller.
 *
 * Some InfosetHandlers do not write to the parse() OutputStream by default
 * because they just create Scala/Java objects or just call interface functions
 * (e.g. SCALA_XML, JDOM). However, for usability, it can be useful to
 * serialize such objects to a String and write to an OutputStream in some
 * circumstances. We do not want to do that in a performance critical loop, so
 * the InfosetHandler parse() function can return a custom InfosetParseResult
 * with a write() implementation to serialize the infoset result to a string
 * and write it to a given OutputStream. Note that this write() function may
 * not ever be called and is guaranteed to occur outside of a performance loop.
 */
sealed class InfosetParseResult(val parseResult: JParseResult) {
  def write(os: OutputStream): Unit = {}
}

/**
 * InfosetType.XML
 */
case class XMLTextInfosetHandler(dataProcessor: JDataProcessor) extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val output = new XMLTextInfosetOutputter(os, pretty = true)
    val pr = parseWithInfosetOutputter(input, output)
    new InfosetParseResult(pr)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val is = data match {
      case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
      case is: InputStream => is
    }
    val input = new XMLTextInfosetInputter(is)
    val ur = unparseWithInfosetInputter(input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = bytes

  def dataToInfoset(stream: InputStream): AnyRef = stream
}

/**
 * InfosetType.JSON
 */
case class JsonInfosetHandler(dataProcessor: JDataProcessor) extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val output = new JsonInfosetOutputter(os, pretty = true)
    val pr = parseWithInfosetOutputter(input, output)
    new InfosetParseResult(pr)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val is = data match {
      case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
      case is: InputStream => is
    }
    val input = new JsonInfosetInputter(is)
    val ur = unparseWithInfosetInputter(input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = bytes

  def dataToInfoset(stream: InputStream): AnyRef = stream
}

/**
 * InfosetType.JDOM
 */
case class JDOMInfosetHandler(dataProcessor: JDataProcessor) extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val output = new JDOMInfosetOutputter()
    val pr = parseWithInfosetOutputter(input, output)
    new JDOMInfosetParseResult(pr, output)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val doc = data.asInstanceOf[org.jdom2.Document]
    val input = new JDOMInfosetInputter(doc)
    val ur = unparseWithInfosetInputter(input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = dataToInfoset(new ByteArrayInputStream(bytes))

  def dataToInfoset(stream: InputStream): AnyRef = {
    val builder = new org.jdom2.input.SAXBuilder() {
      override protected def createParser(): XMLReader = {
        val rdr = super.createParser()
        XMLUtils.setSecureDefaults(rdr)
        rdr
      }
    }
    val doc = builder.build(stream)
    doc
  }
}

class JDOMInfosetParseResult(parseResult: JParseResult, output: JDOMInfosetOutputter)
  extends InfosetParseResult(parseResult) {

  override def write(os: OutputStream): Unit = {
    new org.jdom2.output.XMLOutputter().output(output.getResult(), os)
  }
}

/**
 * InfosetType.SCALA_XML
 */
case class ScalaXMLInfosetHandler(dataProcessor: JDataProcessor) extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val output = new ScalaXMLInfosetOutputter()
    val pr = parseWithInfosetOutputter(input, output)
    new ScalaXMLInfosetParseResult(pr, output)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val node = data.asInstanceOf[scala.xml.Node]
    val input = new ScalaXMLInfosetInputter(node)
    val ur = unparseWithInfosetInputter(input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = dataToInfoset(new ByteArrayInputStream(bytes))

  def dataToInfoset(stream: InputStream): AnyRef = {
    val parser: SAXParser = {
      val f = DaffodilSAXParserFactory()
      f.setNamespaceAware(false)
      val p = f.newSAXParser()
      p
    }
    val node = scala.xml.XML.withSAXParser(parser).load(stream)
    node
  }
}

class ScalaXMLInfosetParseResult(parseResult: JParseResult, output: ScalaXMLInfosetOutputter)
  extends InfosetParseResult(parseResult) {

  override def write(os: OutputStream): Unit = {
    val writer = new java.io.OutputStreamWriter(os, StandardCharsets.UTF_8)
    scala.xml.XML.write(writer, output.getResult(), "UTF-8", true, null)
    writer.flush()
  }
}

/**
 * InfosetType.W3CDOM
 */
case class W3CDOMInfosetHandler(dataProcessor: JDataProcessor) extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val output = new W3CDOMInfosetOutputter()
    val pr = parseWithInfosetOutputter(input, output)
    new W3CDOMInfosetParseResult(pr, output)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val doc = data.asInstanceOf[ThreadLocal[org.w3c.dom.Document]].get
    val input = new W3CDOMInfosetInputter(doc)
    val ur = unparseWithInfosetInputter(input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = {
    // W3C Documents are not thread safe. So create a ThreadLocal so each
    // thread gets its own DOM tree. This has the unfortunate downside that we
    // don't actually convert the XML bytes to DOM until the first call to
    // unparse(), and we'll parse it multiple times if there are multiple
    // threads.
    val doc = new ThreadLocal[org.w3c.dom.Document] {
      override def initialValue = {
        val dbf = DocumentBuilderFactory.newInstance()
        dbf.setNamespaceAware(true)
        dbf.setFeature(XMLUtils.XML_DISALLOW_DOCTYPE_FEATURE, true)
        val db = dbf.newDocumentBuilder()
        db.parse(new ByteArrayInputStream(bytes))
      }
    }
    doc
  }

  def dataToInfoset(stream: InputStream): AnyRef = dataToInfoset(IOUtils.toByteArray(stream))
}

class W3CDOMInfosetParseResult(parseResult: JParseResult, output: W3CDOMInfosetOutputter)
  extends InfosetParseResult(parseResult) {

  override def write(os: OutputStream): Unit = {
    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    val result = new StreamResult(os)
    val source = new DOMSource(output.getResult())
    transformer.transform(source, result)
  }
}

/**
 * InfosetType.NULL
 */
case class NULLInfosetHandler(dataProcessor: JDataProcessor) extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val output = new NullInfosetOutputter()
    val pr = parseWithInfosetOutputter(input, output)
    new InfosetParseResult(pr)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val events = data.asInstanceOf[Array[NullInfosetInputter.Event]]
    val input = new NullInfosetInputter(events)
    val ur = unparseWithInfosetInputter(input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = dataToInfoset(new ByteArrayInputStream(bytes))

  def dataToInfoset(stream: InputStream): AnyRef = {
    val events = NullInfosetInputter.toEvents(stream)
    events
  }
}

/**
 * InfosetType.SAX
 */
case class SAXInfosetHandler(dataProcessor: JDataProcessor, forPerformance: Boolean)
  extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val contentHandler =
      if (forPerformance) {
        new DefaultHandler() // ignores all SAX events
      } else {
        new DaffodilParseOutputStreamContentHandler(os, pretty = true)
      }

    val pr = parseWithSax(input, contentHandler)
    new InfosetParseResult(pr)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val ur =
      if (forPerformance) {
        val xmlReader = new ReplayingXmlReader(data.asInstanceOf[Array[SaxEvent]])
        unparseWithSax(xmlReader, null, output)
      } else {
        val input = data match {
          case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
          case is: InputStream => is
        }
        val xmlReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
        unparseWithSax(xmlReader, input, output)
      }
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = {
    if (forPerformance) {
      dataToInfoset(new ByteArrayInputStream(bytes))
    } else {
      bytes
    }
  }

  def dataToInfoset(stream: InputStream): AnyRef = {
    if (forPerformance) {
      val contentHandler = new SavingContentHandler
      val xmlReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
      xmlReader.setContentHandler(contentHandler)
      xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
      xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
      xmlReader.parse(new InputSource(stream))
      contentHandler.events.toArray
    } else {
      stream
    }
  }

  class ReplayingXmlReader(events: Array[SaxEvent]) extends XMLReader {
    private var _contentHandler: ContentHandler = _

    override def setContentHandler(contentHandler: ContentHandler): Unit = _contentHandler =
      contentHandler

    override def parse(source: InputSource): Unit = {
      for (event <- events) event.replay(_contentHandler)
    }

    // these functions will never be called by the Daffodil XMLReader used for
    // unparsing with the SAX API, or if they are used it doesn't change how we
    // replay the events. Just leave them unimplemented or no-ops
    // $COVERAGE-OFF$
    override def getContentHandler(): org.xml.sax.ContentHandler = ???
    override def getDTDHandler(): org.xml.sax.DTDHandler = ???
    override def getEntityResolver(): org.xml.sax.EntityResolver = ???
    override def getErrorHandler(): org.xml.sax.ErrorHandler = ???
    override def getFeature(name: String): Boolean = ???
    override def getProperty(name: String): Object = ???
    override def parse(systemId: String): Unit = ???
    override def setDTDHandler(handler: org.xml.sax.DTDHandler): Unit = {}
    override def setEntityResolver(resolver: org.xml.sax.EntityResolver): Unit = {}
    override def setErrorHandler(handler: org.xml.sax.ErrorHandler): Unit = {}
    override def setFeature(name: String, value: Boolean): Unit = {}
    override def setProperty(name: String, value: Any): Unit = {}
    // $COVERAGE-ON$
  }

  class SavingContentHandler extends ContentHandler {
    val events = new ArrayBuffer[SaxEvent]()

    override def characters(ch: Array[Char], start: Int, length: Int): Unit =
      events += SaxEventCharacters(ch.clone(), start, length)

    override def endDocument(): Unit =
      events += SaxEventEndDocument()

    override def endElement(uri: String, localName: String, qName: String): Unit =
      events += SaxEventEndElement(uri, localName, qName)

    override def endPrefixMapping(prefix: String): Unit =
      events += SaxEventEndPrefixMapping(prefix)

    override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
      events += SaxEventIgnorableWhitespace(ch.clone(), start, length)

    override def processingInstruction(target: String, data: String): Unit =
      events += SaxEventProcessingInstruction(target, data)

    override def skippedEntity(name: String): Unit =
      events += SaxEventSkippedEntity(name)

    override def startDocument(): Unit =
      events += SaxEventStartDocument()

    override def startElement(
      uri: String,
      localName: String,
      qName: String,
      atts: Attributes
    ): Unit =
      events += SaxEventStartElement(uri, localName, qName, new AttributesImpl(atts))

    override def startPrefixMapping(prefix: String, uri: String): Unit =
      events += SaxEventStartPrefixMapping(prefix, uri)

    override def setDocumentLocator(locator: Locator): Unit = {}
  }

  trait SaxEvent {
    def replay(h: ContentHandler): Unit
  }

  case class SaxEventCharacters(ch: Array[Char], start: Int, length: Int) extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.characters(ch, start, length)
  }

  case class SaxEventEndDocument() extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.endDocument()
  }

  case class SaxEventEndElement(uri: String, localName: String, qName: String)
    extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.endElement(uri, localName, qName)
  }

  case class SaxEventEndPrefixMapping(prefix: String) extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.endPrefixMapping(prefix)
  }

  case class SaxEventIgnorableWhitespace(ch: Array[Char], start: Int, length: Int)
    extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.ignorableWhitespace(ch, start, length)
  }

  case class SaxEventProcessingInstruction(target: String, data: String) extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.processingInstruction(target, data)
  }

  case class SaxEventSkippedEntity(name: String) extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.skippedEntity(name)
  }

  case class SaxEventStartDocument() extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.startDocument()
  }

  case class SaxEventStartElement(
    uri: String,
    localName: String,
    qName: String,
    atts: Attributes
  ) extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.startElement(uri, localName, qName, atts)
  }

  case class SaxEventStartPrefixMapping(prefix: String, uri: String) extends SaxEvent {
    def replay(h: ContentHandler): Unit = h.startPrefixMapping(prefix, uri)
  }
}

/**
 * InfosetType.EXI and InfosetType.EXISA
 */
object EXIInfosetHandler {

  /** non-schema aware EXI **/
  def apply(dataProcessor: JDataProcessor): InfosetHandler = {
    val exiFactory = createEXIFactory(None)
    EXIInfosetHandler(dataProcessor, exiFactory)
  }

  /** schema aware EXI **/
  def apply(dataProcessor: JDataProcessor, schemaUri: URI): InfosetHandler = {
    val exiFactory = createEXIFactory(Some(schemaUri))
    EXIInfosetHandler(dataProcessor, exiFactory)
  }

  def createEXIFactory(optSchema: Option[URI]): EXIFactory = {
    val exiFactory = DefaultEXIFactory.newInstance
    if (optSchema.isDefined) {
      val grammarFactory = GrammarFactory.newInstance
      val grammar =
        grammarFactory.createGrammars(optSchema.get.toString, DFDLCatalogResolver.get)
      exiFactory.setGrammars(grammar)
    }
    exiFactory
  }
}

case class EXIInfosetHandler(dataProcessor: JDataProcessor, exiFactory: EXIFactory)
  extends InfosetHandler {

  def parse(input: InputSourceDataInputStream, os: OutputStream): InfosetParseResult = {
    val exiResult = new EXIResult(exiFactory)
    exiResult.setOutputStream(os)
    val contentHandler = exiResult.getHandler

    val pr = parseWithSax(input, contentHandler)
    new InfosetParseResult(pr)
  }

  def unparse(data: AnyRef, output: DFDL.Output): JUnparseResult = {
    val input = data match {
      case bytes: Array[Byte] => new ByteArrayInputStream(bytes)
      case is: InputStream => is
    }
    val exiSource = new EXISource(exiFactory)
    val xmlReader = exiSource.getXMLReader
    val ur = unparseWithSax(xmlReader, input, output)
    ur
  }

  def dataToInfoset(bytes: Array[Byte]): AnyRef = bytes

  def dataToInfoset(stream: InputStream): AnyRef = stream
}
