package daffodil.xml

/**
 * Adapted from a question/answer on the Stack Overflow web site.
 *
 * See http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
 */

import org.xml.sax.Locator
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml._
import java.io.File
import java.io.FileDescriptor
import java.net.URL
import daffodil.exceptions.Assert
import javax.xml.parsers.SAXParserFactory
import daffodil.util._
import daffodil.dsom.DiagnosticsProviding
import daffodil.dsom.SchemaSet
import daffodil.dsom.SchemaDocument
import org.apache.xml.resolver.CatalogManager
import org.apache.xerces.xni.parser.XMLInputSource
import org.apache.xml.resolver.Catalog
import daffodil.dsom.SchemaDefinitionError
import daffodil.dsom.SchemaDefinitionWarning
import daffodil.dsom.SchemaComponent
import daffodil.exceptions.ThrowsSDE
import javax.xml.validation.SchemaFactory
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.dom.DOMSource
import org.apache.xerces.dom.DOMInputImpl
import org.w3c.dom.ls.LSInput
import javax.xml.validation.ValidatorHandler
import scala.collection.JavaConverters._

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
    log(Debug("initial catalog files: %s ", catFiles))
    val builtInCatalog = Misc.getRequiredResource("/daffodil-built-in-catalog.xml")
    val newCatFiles = builtInCatalog.toString() :: catFiles
    cm.setCatalogFiles(newCatFiles.mkString(";"))

    val catFilesAfter = cm.getCatalogFiles()
    log(Debug("final catalog files: %s ", catFilesAfter))
    cm
  }

  lazy val delegate = {
    val delegate = new Catalog(cm) {
      // catalogManager.debug.setDebug(100) // uncomment for even more debug output
    }
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
    log(Debug("resolveEntity1: %s", ns))
    //
    if (ns == XMLUtils.XSD_NAMESPACE) {
      if (alreadyResolvingXSD) {
        log(Debug("special resolved to null"))
        return null
      }
    }
    val prior = alreadyResolvingXSD
    val res = try {
      alreadyResolvingXSD = (ns == XMLUtils.XSD_NAMESPACE)
      val resolvedId = delegate.resolveURI(ns)
      log(Debug("resolved to: %s", resolvedId))
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
    log(Debug("resolveResource: nsURI = %s, baseURI = %s, type = %s, publicId = %s, systemId = %s", nsURI, baseURI, type_, publicId, systemId))
    val resolvedId = delegate.resolveURI(nsURI)
    log(Debug("resolved to: %s", resolvedId))
    Assert.invariant(resolvedId != null)
    val input = new DOMInputImpl()
    input.setBaseURI(baseURI)
    val is = try {
      val is = new URL(resolvedId).openStream()
      input.setByteStream(is)
      is
    } catch {
      case e: Exception => {
        log(Debug("resolveResource: Exception %s", e))
        throw e
      }
    }
    log(Debug("resolveResource result inputStream = %s", is))
    input
  }

  override def resolveEntity(publicId: String, systemId: String) = {
    log(Debug("resolveEntity3: publicId = %s, systemId = %s", publicId, systemId))
    Assert.invariantFailed("resolveEntity3 - should not be called")
  }

  /**
   * We don't deal with DTDs at all. So this always returns null
   */
  def getExternalSubset(name: String, baseURI: String) = {
    log(Debug("getExternalSubset: name = %s, baseURI = %s", name, baseURI))
    null
  }

  def resolveEntity(name: String, publicId: String, baseURI: String, systemId: String) = {
    log(Debug("resolveEntity4: name = %s, baseURI = %s, publicId = %s, systemId = %s", name, baseURI, publicId, systemId))
    Assert.invariantFailed("resolveEntity4 - should not be called")
  }
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
      (nsURI == XMLUtils.XSD_NAMESPACE ||
        nsURI == XMLUtils.DFDL_SUBSET_NAMESPACE))
    val isTDMLTestSuiteNode = (label == "testSuite" && nsURI != NoNamespace &&
      nsURI == XMLUtils.TDML_NAMESPACE)
    val isFileRootNode = isXSSchemaNode || isTDMLTestSuiteNode

    // augment the scope with the dafint namespace binding but only
    // for root nodes (to avoid clutter with the long xmlns:dafint="big uri")
    // and only if it isn't already there.
    val scopeWithDafInt =
      if (scope.getPrefix(XMLUtils.INT_NS) == null
        && isFileRootNode)
        NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
      else scope

    val haveFileName = isFileRootNode && fileName != ""

    val asIs = super.createNode(pre, label, attrs, scopeWithDafInt, children)

    val alreadyHasFile = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.FILE_ATTRIBUTE_NAME) != None

    // If there is already a _line_ attribute, then we're reloading something
    // that was probably converted back into a string and written out. 
    // The original line numbers are therefore the ones wanted, not any new
    // line numbers, so we don't displace any line numbers that already existed.

    val alreadyHasLine = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.LINE_ATTRIBUTE_NAME) != None
    val alreadyHasCol = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.COLUMN_ATTRIBUTE_NAME) != None
    Assert.invariant(alreadyHasLine == alreadyHasCol)

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
 * Manages the care and feeding of the Xerces schema-aware
 * XML parser that we use to do XML-Schema validation of the
 * files we are reading.
 *
 */
trait SchemaAwareLoaderMixin {
  // This is a single purpose trait
  self: DaffodilXMLLoader =>

  protected def doValidation: Boolean

  lazy val resolver = new DFDLCatalogResolver

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
    xr.setEntityResolver(resolver) // older API?? is this needed? Doesn't seem to hurt.
    xr.setProperty(
      "http://apache.org/xml/properties/internal/entity-resolver",
      resolver)
    parser
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

  override def load(url: URL) = {
    adapter.fileName = url.toURI.toASCIIString
    val res = super.load(url)
    res
  }

  //
  // This is the common routine called by all the load calls to actually 
  // carry out the loading of the schema.
  //
  override def loadXML(source: InputSource, p: SAXParser): Node = {
    // System.err.println("loadXML")
    val xr = p.getXMLReader()
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
