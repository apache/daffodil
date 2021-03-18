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

package org.apache.daffodil.xml

/**
 * Adapted from a question/answer on the Stack Overflow web site.
 *
 * See http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
 */

import java.io.BufferedInputStream
import java.io.File
import java.io.InputStream
import java.io.Reader
import java.net.URI
import javax.xml.XMLConstants
import javax.xml.transform.sax.SAXSource
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.xml.Elem
import scala.xml.InputSource
import scala.xml.Node
import scala.xml.SAXParseException
import scala.xml.SAXParser
import scala.xml.TopScope
import scala.xml.parsing.NoBindingFactoryAdapter
import org.w3c.dom.ls.LSInput
import org.apache.daffodil.api.DaffodilSchemaSource
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.Misc
import org.apache.xerces.xni.parser.XMLInputSource
import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager

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
        try {
          val resourceAsStream = new BufferedInputStream(uri.toURL.openStream())
          val input = new InputStreamLSInput(publicId, uri.toString, resourceAsStream)
          input
        } catch {
          case _: java.io.IOException => null
        }
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

  /**
   * This resolver is also called sometimes. If we pass this resolver to other uses of resolvers
   * (xerces, IBM DFDL, etc.) this API may be called even though Daffodil itself doesn't (or didn't
   * anyway as of when this was written) use this method..
   */
  def resolveEntity(name: String, publicId: String, baseURI: String, systemId: String) = {
    //
    // When this method is called from IBM DFDL, for an xs:include with a schemaLocation, the
    // schemaLocation attribute's value is passed in the systemID string.
    //
    // Ex: publicId = http://example.com,
    // baseURI= file:/tmp/s_9828982379827.dfdl.xsd (generated schema file when a
    // schema is embedded inside a TDML file.
    // and
    // systemID = org/apache/daffodil/xml/DFDLGeneralFormat.dfdl.xsd
    //
    val optURI = resolveCommon(publicId, systemId, baseURI)
    optURI match {
      case None => null
      case Some(uri) => {
        val xis = new InputSource(uri.toURL().openStream())
        xis.setSystemId(uri.toString())
        xis
      }
    }
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

/**
 * This LSInput implementation is tailored specifically for Daffodil's use with
 * Xerces and has implementation details to ensure XML data is read correctly
 * and InputStreams are closed.
 *
 * It is important here that the different data getters (e.g.
 * getCharacterStream, getStringData, getEncoding) do not return a value. Only
 * getByteStream should return a value representing the data. This ensures that
 * Xerces will use the data from the InputStream, use the XML preamble to
 * determine encoding, and close the InputStream upon completion. If any of the
 * other getters return a value, Xerces might ignore the InputStream
 * completely, which can lead to open file descriptors or errors in XML decoding.
 */
class InputStreamLSInput(var pubId: String, var sysId: String, inputStream: InputStream)
  extends LSInput {

  var myBaseURI: String = null

  def getBaseURI = myBaseURI
  def getPublicId = pubId
  def getSystemId = sysId

  def setBaseURI(baseURI: String) = myBaseURI = baseURI
  def setPublicId(publicId: String) = pubId = publicId
  def setSystemId(systemId: String) = sysId = systemId

  def getByteStream = inputStream
  def getCertifiedText = false
  def getCharacterStream = null
  def getEncoding = null
  def getStringData = null

  def setByteStream(byteStream: InputStream) = {
    //do nothing
  }
  def setCertifiedText(certifiedText: Boolean) = {
    //do nothing
  }
  def setCharacterStream(characterStream: Reader) = {
    //do nothing
  }
  def setEncoding(encoding: String) = {
    //do nothing
  }
  def setStringData(stringData: String) = {
    //do nothing
  }
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
  // There are no calls to this in Daffodil code base as of this writing, but
  // the base class scala.xml.factory.XMLLoader calls it.
  //
  override def loadXML(source: InputSource, p: SAXParser): Node = {
    // cannot setSecureDefaults on parser. Must set it on reader created from parser.
    val xr = p.getXMLReader()
    XMLUtils.setSecureDefaults(xr)
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

  def resolver = DFDLCatalogResolver.get

  override lazy val parser: SAXParser = {
    val f = new org.apache.xerces.jaxp.SAXParserFactoryImpl()
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
    // cannot setSecureDefaults on parser. Must set on the xml reader created from it.
    val xr = parser.getXMLReader()
    XMLUtils.setSecureDefaults(xr)
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
  protected val sf = new org.apache.xerces.jaxp.validation.XMLSchemaFactory()
  // XMLSchemaFactory doesn't support secure defaults // XMLUtils.setSecureDefaults(sf)
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
   *
   * Due to limitations in the xerces newSchema() method
   * this method should be called only after loading
   * the schema as a regular XML file, which itself insists
   * on the XMLUtils.setSecureDefaults, so we don't need to
   * further check that here.
   */
  def validateSchema(source: DaffodilSchemaSource): Unit = {
    val inputSource = source.newInputSource()
    val saxSource = new SAXSource(inputSource)
    //
    // We would like this saxSource to be created from an XMLReader
    // so that we can call XMLUtils.setSecureDefaults on it.
    // but we get strange errors if I do that, where every symbol
    // in the schema has an unrecognized namespace prefix.
    //
    sf.newSchema(saxSource)
    inputSource.getByteStream().close()
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

  def xercesAdapter = new DFDLXercesAdapter(errorHandler)

  //
  // Controls whether we setup Xerces for validation or not.
  //
  final var doValidation: Boolean = true

  def setValidation(flag: Boolean): Unit = {
    doValidation = flag
  }

  /**
   * Does (optional) validation,
   */
  def load(source: DaffodilSchemaSource): scala.xml.Node = {
    if (doValidation) {
      val inputSource = source.newInputSource()
      val xercesNode = xercesAdapter.load(inputSource) // validates
      inputSource.getByteStream().close()

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
    constructingLoader.input.close()
    res
  }

  /**
   * Uses xerces ability to load an XSD as a schema specifically,
   * not only as an XML file.
   *
   * This verifies many things about the schema being well formed and
   * valid.
   *
   * The schema is first loaded as an XML file
   * which checks basic XML well-formedness, and for
   * security issues like containing DOCTYPE declarations which
   * we don't allow.
   */
  def validateSchema(source: DaffodilSchemaSource) = {
    load(source) // validate as XML file with XML Schema for DFDL Schemas
    xercesAdapter.validateSchema(source)
  }
}

/**
 * This is handy to keep around for debugging.
 */
class BasicErrorHandler extends org.xml.sax.ErrorHandler {

  var diagnostics: List[SAXParseException] = Nil
  var hasError: Boolean = false

  def warning(exception: SAXParseException) = {
    diagnostics :+= exception
  }

  def error(exception: SAXParseException) = {
    diagnostics :+= exception
    hasError = true
  }
  def fatalError(exception: SAXParseException) = {
    error(exception)
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
