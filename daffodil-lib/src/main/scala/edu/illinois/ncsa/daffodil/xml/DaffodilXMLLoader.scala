package edu.illinois.ncsa.daffodil.xml

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

/**
 * Adapted from a question/answer on the Stack Overflow web site.
 *
 * See http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
 */

import java.io.File
import java.net.URI
import java.net.URL
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.InputSource
import scala.xml.MetaData
import scala.xml.NamespaceBinding
import scala.xml.Node
import scala.xml.Null
import scala.xml.SAXParseException
import scala.xml.SAXParser
import scala.xml.Text
import scala.xml.TopScope
import scala.xml.parsing.NoBindingFactoryAdapter
import org.apache.xerces.dom.DOMInputImpl
import org.apache.xerces.xni.parser.XMLInputSource
import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager
import org.w3c.dom.ls.LSInput
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import javax.xml.XMLConstants
import javax.xml.parsers.SAXParserFactory
import javax.xml.validation.SchemaFactory
import java.io.InputStream
import java.io.BufferedInputStream
import org.w3c.dom.ls.LSResourceResolver
import java.io.Reader
import java.io.FileInputStream

/**
 * Resolves URI/URL/URNs to loadable files/streams.
 *
 * Uses xml catalogs.
 *
 * The user can specify their own catalogs by putting
 * CatalogManager.properties on the classpath when they run
 * daffodil.
 *
 * You can also turn on/off verbose messaging from the resolver
 * by putting 'verbosity=4' in that same file.
 *
 * In all cases, we get the resource daffodil-built-in-catalog.xml
 * and that gets priority, so that entities we choose to resolve
 * as built-ins are resolved from the Daffodil jars.
 */
class DFDLCatalogResolver
  extends org.apache.xerces.xni.parser.XMLEntityResolver
  with org.w3c.dom.ls.LSResourceResolver
  with org.xml.sax.EntityResolver
  with org.xml.sax.ext.EntityResolver2
  with Logging {

  def catalogFiles = cm.getCatalogFiles().asScala.toList.asInstanceOf[List[String]]
  // Caution: it took a long time to figure out how to use
  // the XML Catalog stuff. Many permutations were attempted
  // so change this next block of code at your peril
  //
  lazy val cm = {
    val cm = new CatalogManager()
    cm.setIgnoreMissingProperties(true)
    cm.setRelativeCatalogs(true)
    //
    // Note: don't edit code to turn this on and off. You can 
    // create CatalogManager.properties file (don't commit/push it tho)
    // and put one line in it 'verbosity=4' and it will do the same
    // as this next line.
    // cm.setVerbosity(4)
    //
    // The user might specify catalogs using the CatalogManager.properties
    // file. So to insure that our catalog "wins" any conflicts with the user's 
    // catalogs, we take any user-specified catalogs and explicitly put 
    // our catalog first in the catalog list, and then set it again.
    //
    val catFiles = cm.getCatalogFiles().toArray.toList.asInstanceOf[List[String]]
    log(LogLevel.Debug, "initial catalog files: %s ", catFiles)
    val builtInCatalog = Misc.getRequiredResource("/daffodil-built-in-catalog.xml")
    val newCatFiles = builtInCatalog.toString() :: catFiles
    cm.setCatalogFiles(newCatFiles.mkString(";"))

    val catFilesAfter = cm.getCatalogFiles()
    log(LogLevel.Debug, "final catalog files: %s ", catFilesAfter)
    cm
  }

  lazy val delegate = {
    val delegate = new Catalog(cm)
    //    delegate.getCatalogManager().debug.setDebug(100) // uncomment for even more debug output
    delegate.setupReaders()
    delegate.loadSystemCatalogs()
    delegate
  }

  /**
   * Flag to let us know we're already inside the resolution of the
   * XML Schema URI. See comment below.
   */
  var alreadyResolvingXSD: Boolean = false

  /**
   * Called by SAX parser of the schema to resolve entities.
   *
   * Why the alreadyResolvingXSD flag?? it's because DFDL Schemas use the XSD namespace,
   * but want it to resolve to the DFDL subset schema, but the DFDL Subset Schema
   * uses the XSD Namespace, and does NOT want it to resolve to the DFDL subset,
   * but rather, to the regular XSD namespace. Actually its more than that.
   * We don't want to bother validating the DFDL Subset schemas every time
   * we load a file. So we special case the XSD namespace URI.
   *
   * Without this special case check, we'll recurse and stack overflow here.
   */
  def resolveEntity(ri: org.apache.xerces.xni.XMLResourceIdentifier): XMLInputSource = {
    val ns = ri.getNamespace()
    log(LogLevel.Debug, "resolveEntity1: %s", ns)
    //
    if (ns == XMLUtils.XSD_NAMESPACE) {
      if (alreadyResolvingXSD) {
        log(LogLevel.Debug, "special resolved to null")
        return null
      }
    }
    val prior = alreadyResolvingXSD
    val res = try {
      alreadyResolvingXSD = (ns == XMLUtils.XSD_NAMESPACE)

      // When a schema imports another schema that has the same target namespace, resolving using the namespace
      // will not give the file being imported, but the file that is specified in the catalog for that namespace.
      // To solve this problem, we are using the ExpandedSystemId, if it is set,
      // because it will contain the full path of the file being imported.
      val resolvedId = {
        if (ri.getExpandedSystemId() != null) {
          try {
            val sysId = ri.getExpandedSystemId()
            if (new URL(sysId).openStream() != null) {
              ri.getExpandedSystemId()
            } else {
              delegate.resolveURI(ns)
            }
          } catch {
            case e: Exception => {
              delegate.resolveURI(ns)
            }
          }
        } else {
          delegate.resolveURI(ns)
        }
      }

      log(LogLevel.Debug, "resolved ns %s to: %s", ns, resolvedId)
      if (resolvedId == null) return null
      val res = new XMLInputSource(ri.getPublicId(),
        resolvedId,
        ri.getBaseSystemId())
      res
    } finally {
      alreadyResolvingXSD = prior
    }
    res
  }

  def resolveURI(uri: String) = delegate.resolveURI(uri)

  def resolveResource(type_ : String, nsURI: String, publicId: String, systemId: String, baseURI: String): LSInput = {
    log(LogLevel.Debug, "resolveResource: nsURI = %s, baseURI = %s, type = %s, publicId = %s, systemId = %s", nsURI, baseURI, type_, publicId, systemId)

    val resolvedUri = delegate.resolveURI(nsURI)
    val resolvedSystem = delegate.resolveSystem(systemId)

    // An Include in a schema with a target namespace should resolve to the systemId and ignore the nsURI
    // because the nsURI will resolve to the including schema file.
    // This will cause the including schema to be repeatedly parsed resulting in a stack overflow.

    val resolvedId = {
      if (resolvedSystem != null && resolvedSystem != resolvedUri) {
        resolvedSystem
      } else if (resolvedUri != null && ((systemId == null) || (systemId != null && resolvedUri.endsWith(systemId)))) {
        resolvedUri
      } else
        null // We were unable to resolve the file based on the URI or systemID, so we will return null. 
    }

    val result: LSInput = (resolvedId, systemId) match {
      case (null, null) => Assert.invariantFailed("resolvedId and systemId were null.")
      case (null, sysId) => {

        // Attempt to resolve using the classpath
        val resourceAsStream: InputStream = this.getClass().getClassLoader().getResourceAsStream(sysId)

        if (resourceAsStream != null) {
          val input = Input(publicId, sysId, resourceAsStream)
          val v = this.getClass().getClassLoader().getResource(sysId).toString().replace(sysId, "")
          input.setBaseURI(v)
          input
        } else if (baseURI != null) {
          // Try resolving relative to the including schema file
          val absURI = (new URL(new URI(baseURI).toURL, sysId)).toURI
          val absFile = new File(absURI)
          if (absFile.exists()) {
            val input = new Input(publicId, sysId, new BufferedInputStream(new FileInputStream(absFile)))
            input.setBaseURI(absURI.toString().replace(sysId, ""))
            input
          } else {
            // File does not exist.
            null // null means we wern't able to resolve
          }
        } else {
          null // We wern't able to resolve
        }
      }

      // Resolved using the schema catalog
      case (resolved, _) => {
        val input = new DOMInputImpl()
        input.setBaseURI(baseURI)
        val is = try {
          val is = new URL(resolvedId).openStream()
          input.setByteStream(is)
          is
        } catch {
          case e: Exception => {
            log(LogLevel.Debug, "resolveResource: Exception %s", e)
            throw e
          }
        }
        log(LogLevel.Debug, "resolveResource result inputStream = %s", is)
        input
      }
    }
    log(LogLevel.Debug, "resolved to: %s", result)

    result
  }

  override def resolveEntity(publicId: String, systemId: String) = {
    log(LogLevel.Debug, "resolveEntity3: publicId = %s, systemId = %s", publicId, systemId)
    Assert.invariantFailed("resolveEntity3 - should not be called")
  }

  /**
   * We don't deal with DTDs at all. So this always returns null
   */
  def getExternalSubset(name: String, baseURI: String) = {
    log(LogLevel.Debug, "getExternalSubset: name = %s, baseURI = %s", name, baseURI)
    null
  }

  def resolveEntity(name: String, publicId: String, baseURI: String, systemId: String) = {
    log(LogLevel.Debug, "resolveEntity4: name = %s, baseURI = %s, publicId = %s, systemId = %s", name, baseURI, publicId, systemId)
    Assert.invariantFailed("resolveEntity4 - should not be called")
  }
}

class ResourceResolver
  extends LSResourceResolver {
  def resolveResource(theType: String, namespaceURI: String, publicId: String, systemId: String, baseURI: String): LSInput = {
    val resourceAsStream: InputStream = this.getClass().getClassLoader().getResourceAsStream(systemId)
    val result = Input(publicId, systemId, resourceAsStream)
    result
  }
}

object Input {
  def apply(publicId: String, systemId: String, input: InputStream) = new Input(publicId, systemId, new BufferedInputStream(input))
}

class Input(var pubId: String, var sysId: String, var inputStream: BufferedInputStream)
  extends LSInput {

  var myBaseURI: String = null

  def getPublicId = pubId
  def setPublicId(publicId: String) = pubId = publicId
  def getBaseURI = myBaseURI
  def getByteStream = null
  def getCertifiedText = false
  def getCharacterStream = null
  def getEncoding = null
  def getStringData = {
    this.synchronized {
      val input: Array[Byte] = new Array[Byte](inputStream.available())
      inputStream.read(input)
      val contents = new String(input)
      contents
    }
  }
  def setBaseURI(baseURI: String) = myBaseURI = baseURI
  def setByteStream(byteStream: InputStream) = {}
  def setCertifiedText(certifiedText: Boolean) = {}
  def setCharacterStream(characterStream: Reader) = {}
  def setEncoding(encoding: String) = {}
  def setStringData(stringData: String) = {}
  def getSystemId = sysId
  def setSystemId(systemId: String) = sysId = systemId
  def getInputStream: BufferedInputStream = inputStream
}

/**
 * An Adapter in SAX parsing is both an XMLLoader, and a handler of events.
 */
class DFDLXMLLocationAwareAdapter
  extends NoBindingFactoryAdapter
  with Logging {

  var fileName: String = ""

  var saxLocator: org.xml.sax.Locator = _

  // Get location
  override def setDocumentLocator(locator: org.xml.sax.Locator) {
    //    println("setDocumentLocator line=%s col=%s sysID=%s, pubID=%s".format(
    //      locator.getLineNumber, locator.getColumnNumber,
    //      locator.getSystemId, locator.getPublicId))
    this.saxLocator = locator
    super.setDocumentLocator(locator)
  }

  case class Locator(line: Int, col: Int, sysID: String, pubID: String)

  // Without a trick, locators will always provide the end position of an element
  // and we want the start position.
  // With this trick, the line and column will be of the ending ">" of the
  // starting element tag.

  // startElement saves locator information on stack
  val locatorStack = new scala.collection.mutable.Stack[Locator]
  // endElement pops it off into here
  var elementStartLocator: Locator = _

  // create node then uses it.
  override def createNode(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding, children: List[Node]): Elem = {

    // If we're the xs:schema node, then append attribute for _file_ as well.

    val nsURI = NS(scope.getURI(pre))
    val isXSSchemaNode = (label == "schema" && nsURI != NoNamespace &&
      (nsURI == XMLUtils.XSD_NAMESPACE))
    val isTDMLTestSuiteNode = (label == "testSuite" && nsURI != NoNamespace &&
      nsURI == XMLUtils.TDML_NAMESPACE)
    val isFileRootNode = isXSSchemaNode || isTDMLTestSuiteNode

    // augment the scope with the dafint namespace binding but only
    // for root nodes (to avoid clutter with the long xmlns:dafint="big uri")
    // and only if it isn't already there.
    //
    // The above would be a nice idea, but it requires that we are recursively
    // descending & ascending, carrying along that namespace definition so that
    // we have encountered the root first, etc.
    // Because Nodes are immutable, they don't point up at the scope toward
    // the parent. 
    // 
    // So we append this NS binding regardless of whether it is root or not.
    // Though we don't if it is already there.
    // 
    lazy val scopeWithDafInt =
      if (scope.getPrefix(XMLUtils.INT_NS) == null) // && isFileRootNode
        NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
      else scope

    val haveFileName = isFileRootNode && fileName != ""

    val alreadyHasFile = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.FILE_ATTRIBUTE_NAME) != None

    // If there is already a _line_ attribute, then we're reloading something
    // that was probably converted back into a string and written out. 
    // The original line numbers are therefore the ones wanted, not any new
    // line numbers, so we don't displace any line numbers that already existed.

    val alreadyHasLine = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.LINE_ATTRIBUTE_NAME) != None
    val alreadyHasCol = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.COLUMN_ATTRIBUTE_NAME) != None
    Assert.invariant(alreadyHasLine == alreadyHasCol)

    val newScope =
      if (alreadyHasFile && alreadyHasLine && alreadyHasCol) scope
      else scopeWithDafInt

    val asIs = super.createNode(pre, label, attrs, newScope, children)

    val lineAttr =
      if (alreadyHasLine) Null
      else Attribute(XMLUtils.INT_PREFIX, XMLUtils.LINE_ATTRIBUTE_NAME, Text(elementStartLocator.line.toString), Null)
    val colAttr =
      if (alreadyHasCol) Null
      else Attribute(XMLUtils.INT_PREFIX, XMLUtils.COLUMN_ATTRIBUTE_NAME, Text(elementStartLocator.col.toString), Null)
    val fileAttr =
      if (alreadyHasFile || !haveFileName) Null
      else {
        val fileURIProtocolPrefix = if (fileName.startsWith("file:")) "" else "file:"
        Attribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, Text(fileURIProtocolPrefix + fileName), Null)
      }

    // Scala XML note: The % operator creates a new element with updated attributes
    val res = asIs % lineAttr % colAttr % fileAttr
    // System.err.println("Create Node: " + res)
    res
  }

  override def startElement(uri: String, _localName: String, qname: String, attributes: org.xml.sax.Attributes): Unit = {
    // println("startElement")
    val loc = Locator(saxLocator.getLineNumber, saxLocator.getColumnNumber, saxLocator.getSystemId, saxLocator.getPublicId)
    locatorStack.push(loc)
    super.startElement(uri, _localName, qname, attributes)
  }

  override def endElement(uri: String, _localName: String, qname: String): Unit = {
    // println("endElement")
    elementStartLocator = locatorStack.pop
    super.endElement(uri, _localName, qname)
  }

}

/**
 * Generally singletons in daffodil should be objects that are stored
 * in the SchemaSet, the DataProcessor or the ProcessorFactory. With
 * this singleton, Daffodil cannot run two different schemas that use
 * two different XML Catalogs to resolve their schema includes/imports
 * in the same JVM.
 *
 * Of course the underlying XML Catalog resolver may have the singleton
 * constraint, if so then there's no point in our stuff avoiding the
 * singleton.
 *
 * I suspect this is in fact the case, so we'll leave this a singleton.
 */
object DaffodilCatalogResolver {
  lazy val resolver = new DFDLCatalogResolver
}

/**
 * Manages the care and feeding of the Xerces schema-aware
 * XML parser that we use to do XML-Schema validation of the
 * files we are reading.
 *
 */
trait SchemaAwareLoaderMixin {
  // This is a single purpose trait
  self: DaffodilXMLLoader =>

  protected def doValidation: Boolean

  protected lazy val resolver = DaffodilCatalogResolver.resolver

  override def parser: SAXParser = {

    // val x = new com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl

    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(true)
    f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)

    if (doValidation) {
      f.setFeature("http://xml.org/sax/features/validation", true)
      f.setFeature("http://apache.org/xml/features/validation/dynamic", true)
      f.setFeature("http://apache.org/xml/features/validation/schema", true)
      f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
      //      f.setValidating(true) // old style API? 
    }
    val parser = f.newSAXParser()
    val xr = parser.getXMLReader()
    xr.setContentHandler(this)
    //xr.setEntityResolver(resolver) // older API?? is this needed? Doesn't seem to hurt.
    xr.setProperty(
      "http://apache.org/xml/properties/internal/entity-resolver",
      resolver)
    parser
  }

  /**
   * UPA errors are detected by xerces if the schema-full-checking feature is
   * turned on, AND if you inform xerces that it is reading an XML Schema
   * (i.e., xsd).
   *
   * We are using it differently than this. We are loading DFDL Schemas,
   * which are being validated not as XML Schemas via xerces built-in
   * mechanisms, but as XML documents having a schema that we provide, which
   * enforces the subset of XML Schema that DFDL uses, etc.
   *
   * The problem is that checks like UPA aren't expressible in a
   * schema-for-DFDL-schemas. They are coded algorithmically right into Xerces.
   *
   * In order to get the UPA checks on DFDL schemas we have to do this in two
   * passes in order for things to be compatible with the TDMLRunner because
   * a TDML file is not a valid schema.
   *
   * First pass: The DFDL schema (or TDML file) is read as an XML document.
   * Second pass: We specially load up the DFDL schemas (in the case
   * of the TDMLRunner the DFDL Schema is extracted and loaded) treating them
   * as XML schemas, just to get these UPA diagnostics.  This is accomplished by
   * using the below SchemaFactory and SchemaFactory.newSchema calls.  The
   * newSchema call is what forces schema validation to take place.
   */
  protected val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
  sf.setResourceResolver(resolver)
  sf.setErrorHandler(errorHandler)

  protected def validateSchemaFile(file: File) = {
    sf.newSchema(file)
  }

}

/**
 * Our modified XML loader.
 *
 * It validates as it loads. (If you ask it to via setting a flag.)
 *
 * It resolves xmlns URIs using an XML Catalog which
 * can be extended to include user-defined catalogs. By way of a
 * CatalogManger.properties file anywhere on the classpath.
 *
 * It adds diagnostic file, line number, column number information
 * to the nodes.
 *
 */
class DaffodilXMLLoader(val errorHandler: org.xml.sax.ErrorHandler)
  extends DFDLXMLLocationAwareAdapter
  with SchemaAwareLoaderMixin {

  //
  // Controls whether we setup Xerces for validation or not.
  // 
  final var doValidation: Boolean = true

  def setValidation(flag: Boolean) {
    doValidation = flag
  }

  def validateDFDLSchema(file: File): Unit = validateSchemaFile(file)

  // everything interesting happens in the callbacks from the SAX parser
  // to the adapter.
  override def adapter = this

  // these load/loadFile overrides so we can grab the filename and give it to our
  // adapter that adds file attributes to the root XML Node.
  override def loadFile(f: File) = {
    adapter.fileName = f.getAbsolutePath()

    val res = super.loadFile(f)
    res
  }

  override def loadFile(name: String) = loadFile(new File(name))

  def load(uri: URI) = {
    adapter.fileName = uri.toASCIIString
    val res = super.load(uri.toURL())
    res
  }

  //
  // This is the common routine called by all the load calls to actually 
  // carry out the loading of the schema.
  //
  override def loadXML(source: InputSource, p: SAXParser): Node = {
    // System.err.println("loadXML")
    val xr = p.getXMLReader()
    //    xr.setFeature("http://apache.org/xml/features/namespace-growth", true)
    xr.setErrorHandler(errorHandler)
    // parse file
    scopeStack.push(TopScope)
    // System.err.println("beginning parse")
    xr.parse(source)
    // System.err.println("ending parse")
    scopeStack.pop
    rootElem.asInstanceOf[Elem]
  }

}

/**
 * This is handy to keep around for debugging.
 */
object BasicStderrErrorHandler extends org.xml.sax.ErrorHandler {

  def warning(exception: SAXParseException) = {
    System.err.println("Warning " + exception.getMessage())
  }

  def error(exception: SAXParseException) = {
    System.err.println("Error: " + exception.getMessage())
  }
  def fatalError(exception: SAXParseException) = {
    System.err.println("Fatal: " + exception.getMessage())
  }
}

abstract class DFDLSchemaValidationException(cause: Throwable) extends Exception(cause)
case class DFDLSchemaValidationWarning(cause: Throwable) extends DFDLSchemaValidationException(cause)
case class DFDLSchemaValidationError(cause: Throwable) extends DFDLSchemaValidationException(cause)

/**
 * DFDLSchemaErrorHandler exists so that we can throw DFDLSchemaValidation specific
 * diagnostics and filter out warnings vs. errors.  This is so that we know when to
 * SDE vs. SDW.
 */
object DFDLSchemaErrorHandler extends org.xml.sax.ErrorHandler {

  // We are escalating all warnings from the schema validation to an error
  def warning(exception: SAXParseException) = error(exception)

  def error(exception: SAXParseException) = {
    throw new DFDLSchemaValidationError(exception)
  }
  def fatalError(exception: SAXParseException) = {
    throw new DFDLSchemaValidationError(exception)
  }
}
