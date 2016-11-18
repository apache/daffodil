/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.xml

/**
 * Adapted from a question/answer on the Stack Overflow web site.
 *
 * See http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
 */

import java.io.File
import java.net.URI
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.xml.Elem
import scala.xml.InputSource
import scala.xml.Node
import scala.xml.SAXParseException
import scala.xml.SAXParser
import scala.xml.TopScope
import scala.xml.parsing.NoBindingFactoryAdapter
import org.apache.xerces.xni.parser.XMLInputSource
import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager
import org.w3c.dom.ls.LSInput
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.api.DaffodilSchemaSource
import javax.xml.XMLConstants
import javax.xml.parsers.SAXParserFactory
import javax.xml.validation.SchemaFactory
import java.io.InputStream
import java.io.BufferedInputStream
import java.io.Reader
import javax.xml.transform.sax.SAXSource

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
class DFDLCatalogResolver private ()
  extends org.apache.xerces.xni.parser.XMLEntityResolver
  with org.w3c.dom.ls.LSResourceResolver
  with org.xml.sax.EntityResolver
  with org.xml.sax.ext.EntityResolver2
  with Logging {

  lazy val init = {
    cm
    catalogFiles
    delegate
  }

  lazy val catalogFiles = cm.getCatalogFiles().asScala.toList.asInstanceOf[List[String]]
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
   *
   * This is not thread safe, but the underlying catalog resolver isn't either, so
   * we're not making it worse.
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
    val nsString = ri.getNamespace()
    val ns = NS(nsString)
    val literalSysId = ri.getLiteralSystemId()
    val baseURIString = ri.getBaseSystemId()

    if (ns == XMLUtils.XSD_NAMESPACE) {
      if (alreadyResolvingXSD) {
        log(LogLevel.Debug, "special resolved to null")
        return null
      }
    }
    val prior = alreadyResolvingXSD
    val res = try {
      alreadyResolvingXSD = (ns == XMLUtils.XSD_NAMESPACE)
      val optURI = resolveCommon(nsString, literalSysId, baseURIString)
      optURI match {
        case None => null
        case Some(uri) => {
          val xis = new XMLInputSource(null, uri.toString, null)
          xis
        }
      }
    } finally {
      alreadyResolvingXSD = prior
    }
    res
  }

  def resolveURI(uri: String): String = {
    init
    val optURI = resolveCommon(uri, null, null)
    optURI match {
      case None => null
      case Some(uri) => uri.toString
    }
  }

  private def resolveCommon(nsURI: String, systemId: String, baseURIString: String): Option[URI] = {
    init
    if (nsURI == null && systemId == null && baseURIString == null) return None

    log(LogLevel.Resolver, "nsURI = %s, baseURI = %s, systemId = %s", nsURI, baseURIString, systemId)
    val resolvedUri = delegate.resolveURI(nsURI)
    val resolvedSystem =
      if (systemId == null) null
      else {
        delegate.resolveSystem(systemId) match {
          case null => {
            val systemIdFile = new File(systemId)
            if (systemIdFile.exists) systemIdFile.toURI().toString else null
          }
          case rSys => rSys
        }
      }

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

    val result = (resolvedId, systemId) match {
      case (null, null) => {
        // This happens now in some unit tests.
        // Assert.invariantFailed("resolvedId and systemId were null.")
        log(LogLevel.Resolver, "Unable to resolve.")
        return None
      }
      case (null, sysId) =>
        {
          val baseURI = if (baseURIString == null) None else Some(new URI(baseURIString))
          val optURI = Misc.getResourceRelativeOption(sysId, baseURI)
          optURI match {
            case Some(uri) => log(LogLevel.Resolver, "Found on classpath: %s.", uri)
            case None => log(LogLevel.Info, "Unable to resolve " + sysId + " in " + baseURI)
          }
          optURI
        }
      case (resolved, _) => {
        log(LogLevel.Resolver, "Found via XML Catalog: %s.", resolved)
        Some(new URI(resolved))
      }
    }
    result
  }

  def resolveResource(type_ : String, nsURI: String, publicId: String, systemId: String, baseURIString: String): LSInput = {
    val optURI = resolveCommon(nsURI, systemId, baseURIString)
    optURI match {
      case None => null
      case Some(uri) => {
        val resourceAsStream =
          try {
            uri.toURL.openStream() // This will work.
          } catch {
            case _: java.io.IOException => Assert.invariantFailed("found resource but couldn't open")
          }
        val input = new Input(publicId, systemId, new BufferedInputStream(resourceAsStream))
        input.setBaseURI(uri.toString)
        input
      }
    }
  }

  override def resolveEntity(publicId: String, systemId: String) = {
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
    Assert.invariantFailed("resolveEntity4 - should not be called")
  }
}

/**
 * catalog resolvers aren't thread safe. But they're also expensive stateful,
 * do I/O etc. so we really only want one per thread.
 */
object DFDLCatalogResolver {
  lazy val d = new ThreadLocal[DFDLCatalogResolver] {
    override def initialValue() = {
      new DFDLCatalogResolver()
    }
  }
  def get = d.get
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
  def setBaseURI(baseURI: String) = {
    Assert.usage(!baseURI.contains("xsd/xsd"))
    myBaseURI = baseURI
  }
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
class DFDLXercesAdapter(val errorHandler: org.xml.sax.ErrorHandler)
  extends NoBindingFactoryAdapter
  with SchemaAwareLoaderMixin
  //
  // We really want a whole XML Stack that is based on SAX2 DefaultHandler2 so that
  // we can handle CDATA elements right (we need events for them so we can
  // create PCData objects from them directly.
  //
  // The above is the only reason the TDML runner has to use the ConstructingParser
  // instead of calling this loader.
  //
  with Logging {

  final val doValidation: Boolean = true

  // everything interesting happens in the callbacks from the SAX parser
  // to the adapter.
  override def adapter = this

  def load(uri: URI): Node = load(uri.toURL)
  override def loadFile(f: File) = load(f.toURI)

  // We disallow any of these except ones where we can definitely get an associated
  // identifier or name from it.
  private def noWay = Assert.usageError("Operation is not supported. Use load(uri) or loadFile(file)")
  override def loadFile(fd: java.io.FileDescriptor): Node = noWay
  override def loadFile(name: String): Node = loadFile(new File(name))
  override def load(is: InputStream): Node = noWay
  override def load(reader: Reader): Node = noWay
  override def load(sysID: String): Node = noWay

  //
  // This is the common routine called by all the load calls to actually
  // carry out the loading of the schema.
  //
  override def loadXML(source: InputSource, p: SAXParser): Node = {

    val xr = p.getXMLReader()
    xr.setErrorHandler(errorHandler)
    scopeStack.push(TopScope)
    xr.parse(source)
    scopeStack.pop
    rootElem.asInstanceOf[Elem]
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
  self: DFDLXercesAdapter =>

  protected def doValidation: Boolean

  lazy val resolver = DFDLCatalogResolver.get

  override lazy val parser: SAXParser = {

    // val x = new com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl

    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(true)
    f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)

    // JIRA DFDL-1659 - make sure not accessing things remotely and protect from denial-of-service
    // using XML trickery.
    // f.setFeature("http://javax.xml.XMLConstants/property/accessExternalDTD", false)
    //    f.setFeature("http://xml.org/sax/features/external-general-entities", false)
    //    f.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    f.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)

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
   * Detecting these requires that we do THREE passes
   * 1) load the DFDL schema as an XML document. This validates it against the XML Schema
   * for DFDL schemas.
   * 2) load the DFDL schema as an XSD - xerces then does lots of more intensive checking
   * of the schema
   * 3) load the schema for our own consumption by Daffodil code. This uses the
   * constructing parser so as to preserve CDATA regions (xerces just does the wrong
   * thing with those,...fatally so). Then our own semantic checking is performed
   * as part of compiling the DFDL schema.
   *
   * Checks like UPA are in step (2) above. They are coded algorithmically
   * right into Xerces. This is accomplished by
   * using the below SchemaFactory and SchemaFactory.newSchema calls.  The
   * newSchema call is what forces schema validation to take place.
   */
  protected val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
  sf.setResourceResolver(resolver)
  sf.setErrorHandler(errorHandler)

  /**
   * This loads the DFDL schema as an XML Schema. This will
   * check many more things about the DFDL schema other than
   * just whether it validates against the XML Schema for DFDL schemas.
   *
   * Unfortunately, we don't have control over how Xerces loads up these schemas
   * (other than the resolver anyway), so we can't really redirect the way
   * it issues error messages so that it properly lays blame at say, the schema fragments
   * inside an embedded schema of a TDML file.
   *
   * So if we want good file/line/column info from this, we have to give
   * it a plain old file or resource, and not try to play games to get it to
   * pick up the file/line/col information from attributes of the elements.
   */
  def validateSchema(source: DaffodilSchemaSource) = {
    val saxSource = new SAXSource(source.newInputSource())
    sf.newSchema(saxSource)
  }

}

/**
 * Our modified XML loader.
 *
 * The saga of the XML Loaders in scala - it's a long story. Xerces is java code.
 * It seems to be hopelessly broken in how it handles CDATA regions. It loses line
 * information in them. Push comes to shove if an XML element contains syntax where
 * line endings play a role. In DFDL, regular expressions that use the free-form comment
 * syntax need the line-endings to be preserved. Xerces always loses them, so the line
 * endings get lost and a free-form regex with a comment is hopelessly broken.
 *
 * Xerces doesn't preserve CDATA and create PCData nodes. Rather, it creates ordinary
 * Text nodes. Nor does it give you any way to get control so
 * that you can handle CDATA properly.  Hence, we use Xerces to validate XML files
 * against XML Schemas, and that includes to validate DFDL schemas against the
 * XML Schema for DFDL schemas. We also use it to validate DFDL schemas as XML Schemas
 * since that checks many things more than just validity checking does.  And we use
 * Xerces for "full validation" of the Infoset results from DFDL parsing, by converting
 * them to XML and validating them as XML against the DFDL Schema (in that case being
 * interpreted only as an XML Schema).
 *
 * A second issue: some DFDL schemas are created programatically from other files such
 * as TDML files. It would be great if we could redirect Xerces to use file/line/col
 * information from the TDML files instead of its own. But this has proven to be
 * fragile and depends on much undocumented behavior, so was removed. One might get
 * it to work for xml.load(...), but there's also the XSD validation of the schema,
 * and there we have even less visibility to the parsing of the schema.
 *
 * Back to the CDATA issue: Unfortunately, in order to actually get the XML nodes we need for TDML or DFDL
 * schemas where CDATA regions may be present, we can't use Xerces. So Daffodil has its
 * own DaffodilConstructingLoader which uses the scala.xml.ConstructingParser
 * for XML which does deal with CDATA regions properly. Daffodil extends
 * the Scala library constructing parser so as to capture the file/line/col
 * information needed for diagnostic messages.
 *
 * The DaffodilXMLLoader also resolves xmlns URIs using an XML Catalog which
 * can be extended to include user-defined catalogs. By way of a
 * CatalogManger.properties file anywhere on the classpath.
 *
 * It adds the diagnostic file, line number, column number information
 * to the nodes unless such info is already present.
 *
 * The DaffodilConstructingLoader doesn't do any resolving of its own (it does not
 * do any validation either), however, once Daffodil starts processing the
 * DFDL schema nodes, it resolves references using the same one true XML catalog resolver.
 */
class DaffodilXMLLoader(val errorHandler: org.xml.sax.ErrorHandler) {

  def this() = this(RethrowSchemaErrorHandler)

  val xercesAdapter = new DFDLXercesAdapter(errorHandler)

  //
  // Controls whether we setup Xerces for validation or not.
  //
  final var doValidation: Boolean = true

  def setValidation(flag: Boolean) {
    doValidation = flag
  }

  /**
   * Does (optional) validation,
   */
  def load(source: DaffodilSchemaSource): scala.xml.Node = {
    var xercesNode: Node = null
    if (doValidation) {
      xercesNode = xercesAdapter.load(source.newInputSource()) // validates
      if (xercesNode == null) return null
      // Note: we don't call xercesAdapter.validateSchema(source)
      // here, because this is an XML loader, not necessarily
      // just a DFDL schema loader. So for example the doValidation flag
      // above could be telling us to validate a TDML file or not.
    }
    //
    // To get reliable xml nodes including conversion of CDATA syntax into
    // PCData nodes, we have to use a different loader.
    //
    val constructingLoader = new DaffodilConstructingLoader(source.uriForLoading, errorHandler)
    val res = constructingLoader.load() // construct the XML objects for us.
    res
  }

  def validateSchema(source: DaffodilSchemaSource) = xercesAdapter.validateSchema(source)
}

/**
 * This is handy to keep around for debugging.
 */
class BasicErrorHandler extends org.xml.sax.ErrorHandler {

  var diagnostics: List[SAXParseException] = Nil
  var hasError: Boolean = false

  def warning(exception: SAXParseException) = {
    diagnostics :+= exception
    // System.err.println("Warning " + exception.getMessage())
  }

  def error(exception: SAXParseException) = {
    diagnostics :+= exception
    hasError = true
    // System.err.println("Error: " + exception.getMessage())
  }
  def fatalError(exception: SAXParseException) = {
    diagnostics :+= exception
    hasError = true
    // System.err.println("Fatal: " + exception.getMessage())
  }
}

abstract class DFDLSchemaValidationException(cause: Throwable) extends Exception(cause)
case class DFDLSchemaValidationWarning(cause: Throwable) extends DFDLSchemaValidationException(cause)
case class DFDLSchemaValidationError(cause: Throwable) extends DFDLSchemaValidationException(cause)

object RethrowSchemaErrorHandler extends org.xml.sax.ErrorHandler {
  def warning(exception: SAXParseException) = {
    throw exception
  }

  def error(exception: SAXParseException) = {
    throw exception
  }

  def fatalError(exception: SAXParseException) = {
    throw exception
  }
}
