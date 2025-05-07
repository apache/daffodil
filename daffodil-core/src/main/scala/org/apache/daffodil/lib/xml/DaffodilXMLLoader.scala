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

package org.apache.daffodil.lib.xml

/**
 * Adapted from a question/answer on the Stack Overflow web site.
 *
 * See http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
 */

import java.io.BufferedInputStream
import java.io.InputStream
import java.io.Reader
import java.net.URI
import java.nio.file.Paths
import javax.xml.XMLConstants
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import javax.xml.validation.SchemaFactory
import scala.jdk.CollectionConverters._
import scala.xml.SAXParseException
import scala.xml.SAXParser
import scala.xml.parsing.NoBindingFactoryAdapter

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilSchemaSource
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.validation.XercesValidator

import org.apache.xerces.jaxp.validation.XMLSchemaFactory
import org.apache.xerces.xni.parser.XMLInputSource
import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager
import org.w3c.dom.ls.LSInput
import org.xml.sax.ContentHandler
import org.xml.sax.DTDHandler
import org.xml.sax.EntityResolver
import org.xml.sax.ErrorHandler
import org.xml.sax.InputSource
import org.xml.sax.XMLReader
import org.xml.sax.helpers.XMLFilterImpl

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
  with javax.xml.transform.URIResolver {

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
    Logger.log.debug(s"initial catalog files: ${catFiles}")
    val builtInCatalog = Misc.getRequiredResource("/daffodil-built-in-catalog.xml")
    val newCatFiles = builtInCatalog.toString() :: catFiles
    cm.setCatalogFiles(newCatFiles.mkString(";"))

    val catFilesAfter = cm.getCatalogFiles()
    Logger.log.debug(s"final catalog files: ${catFilesAfter}")
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
        Logger.log.debug(s"special resolved to null")
        return null
      }
    }
    val prior = alreadyResolvingXSD
    val res =
      try {
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
    val optURI = resolveCommon(uri, null, null)
    optURI match {
      case None => null
      case Some(uri) => uri.toString
    }
  }

  override def resolve(href: String, base: String): Source = {
    val optURI = resolveCommon(null, href, base)
    // The way resolveCommon is called here, it should always throw an exception during
    // resolution failure, so we should never get a None. That exception should be handled by
    // the caller, so we can assume we have an URI at this point.
    Assert.invariant(optURI.isDefined)
    val uri = optURI.get
    val source = new StreamSource(uri.toURL.openStream)
    source.setSystemId(uri.toString)
    source
  }

  private def resolveCommon(
    nsURI: String,
    systemId: String,
    baseURIString: String
  ): Option[URI] = {
    init
    if (nsURI == null && systemId == null && baseURIString == null) return None

    Logger.log.debug(s"nsURI = ${nsURI}, baseURI = ${baseURIString}, systemId = ${systemId}")
    val resolvedUri = delegate.resolveURI(nsURI)
    val resolvedSystem = delegate.resolveSystem(systemId)

    // An Include in a schema with a target namespace should resolve to the systemId and ignore the nsURI
    // because the nsURI will resolve to the including schema file.
    // This will cause the including schema to be repeatedly parsed resulting in a stack overflow.

    lazy val systemIdUri = if (systemId != null) {
      new URI(systemId)
    } else {
      null
    }

    /**
     * Xerces has a bug where it absolutizes systemId i.e the user supplies
     *  {{{
     *    <xs:schema...
     *    ... xsi:schemaLocation="urn:some:namespace /some/path.xsd"
     *  }}}
     * Xerces takes that schemaLocation URI and absolutizes it to {{{ file:/some/path.xsd }}}
     * and passes that to our resolveEntity and in turn resolveCommon, which while it's able
     * to find the namespace, fails to set the resolvedUri since the file:/some/path.xsd will
     * never match anything resolved from our catalog since that'd return something like
     * {{{ file:/some/absolute/path/to/some/path.xsd }}}
     *
     * This is a workaround to that bug where we convert systemId to a URI and check if the
     * path (from URI.getPath) matches the end of resolvedUri. Note: This can ignore absolute
     * URIs passed in for schemaLocation, but those are edge cases where the user expects
     * the namespace to match a different file (i.e what they provide in the schemalocation)
     * than what we find in the catalog.
     */
    lazy val systemIdPath = if (systemIdUri != null && systemIdUri.getScheme == "file") {
      systemIdUri.getPath
    } else {
      systemId
    }
    val resolvedId = {
      if (resolvedSystem != null && resolvedSystem != resolvedUri) {
        resolvedSystem
      } else if (
        resolvedUri != null && ((systemId == null) || (systemId != null && resolvedUri.endsWith(
          systemIdPath
        )))
      ) {
        resolvedUri
      } else
        null // We were unable to resolve the file based on the URI or systemID, so we will return null.
    }

    val result = (resolvedId, systemId) match {
      case (null, null) => {
        // This happens in numerous unit tests.
        //
        // It seems that in some situations the resolver is called
        // to attempt to resolve things certain ways. Such as
        // providing just the namespace URI, without the systemId.
        //
        // So the inability to resolve, in this case anyway, is not an error.
        //
        None
      }
      case (null, sysId) => {
        // We did not get a catalog resolution of the nsURI. We now look for the systemID (which
        // comes from the schemaLocation attribute) on classpath or as a file.
        val optURI =
          try {
            val baseURI = new URI(baseURIString)
            // DaffodilXMLLoader implements a Xerces API, and Xerces doesn't have a concept of diagnostic path
            // that we need for a URISchemaSource to pass in as the context for resolveSchemaLocation.
            // We could use some heuristic to come up with a diagnostic path (e.g. call uriToDiagosticPath).
            // But we don't actually use the diagnosticPath returned by resolveSchemaLocation here, all we care
            // about is getting a URI for Xerces. So the diagnostic path in the schema source doesn't really
            // matter as long as it doesn't break resolveSchemaLocation, and an empty diagnostic path works
            // fine for that.
            val uriSchemaSource = URISchemaSource(Paths.get("").toFile, baseURI)
            val contextURI = Some(uriSchemaSource)
            val resolved = XMLUtils.resolveSchemaLocation(sysId, contextURI)
            // we drop the boolean return part of resolveSchemaLocation because we don't care here
            // if a relative schemaLocation was resolved absolutely. Daffodil will detect that
            // elsewhere and output a warning.
            resolved.map(_._1.uri)
          } catch {
            case e: IllegalArgumentException =>
              throw new SAXParseException(
                s"Invalid or unsupported schemaLocation URI: ${e.getMessage}",
                null
              )
          }

        optURI match {
          case Some(uri) => Logger.log.debug(s"Found schemaLocation: ${uri}.")
          case None => {
            // We have to explicitly throw this, because returning with a no-resolve does not
            // cause Xerces to report an error. Instead you just get later errors about symbols
            // that can't be resolved, but it never mentions that an include/import didn't work.
            throw new SAXParseException(s"Unable to resolve schemaLocation: $systemId", null)
          }
        }
        optURI
      }
      case (resolved, _) => {
        Logger.log.debug(s"Found via XML Catalog: ${resolved}.")
        Some(new URI(resolved))
      }
    }
    result
  }

  def resolveResource(
    type_ : String,
    nsURI: String,
    publicId: String,
    systemId: String,
    baseURIString: String
  ): LSInput = {
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
    Logger.log.debug(s"getExternalSubset: name = ${name}, baseURI = ${baseURI}")
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
    // do nothing
  }
  def setCertifiedText(certifiedText: Boolean) = {
    // do nothing
  }
  def setCharacterStream(characterStream: Reader) = {
    // do nothing
  }
  def setEncoding(encoding: String) = {
    // do nothing
  }
  def setStringData(stringData: String) = {
    // do nothing
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
class DaffodilXMLLoader(val errorHandler: org.xml.sax.ErrorHandler)
  extends NoBindingFactoryAdapter {

  def this() = this(RethrowSchemaErrorHandler)

  private def resolver = DFDLCatalogResolver.get

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
   *
   * Note that this is intentionally a def rather than a val because an
   * XMLSchemaFactory actually holds on to many large objects used when loading
   * a schema, which can be massive memory leak. By making it a def, this
   * factory and all its references can be garbage collected after a schema is
   * loaded. And the schemaFactory is only used when validating a DFDL schema,
   * so we should only create one factory per DaffodilXMLLoader, so saving it
   * as val does not gain anything.
   */
  private def schemaFactory: XMLSchemaFactory = {
    val sf = new XMLSchemaFactory()
    sf.setResourceResolver(resolver)
    //
    // despite setting the errorHandler here, the validator
    // sometimes still throws exceptions, so we cannot depend
    // exclusively on the errorHandler picking off all errors.
    //
    // In particular, fatal errors when a node cannot be returned
    // from the parse, always cause a throw.
    //
    sf.setErrorHandler(errorHandler)
    sf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    sf.setFeature(XMLUtils.XML_DISALLOW_DOCTYPE_FEATURE, true)
    // These are not recognized by a schemaFactory
    // sf.setFeature("http://xml.org/sax/features/validation", true)
    // sf.setFeature("http://apache.org/xml/features/validation/schema", true)
    // sf.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
    sf
  }

  /**
   * checks that a DFDL schema file, viewed as an XML document is valid
   * relative to the XML Schema for DFDL Schemas.
   *
   * For example this ensures that you can only have minOccurs and maxOccurs
   * on local element declarations and element refs, not on sequence/choice/group.
   */
  def validateAsIndividualDFDLSchemaFile(source: DaffodilSchemaSource): Unit = {
    // first we load it, with validation explicitly against the
    // schema for DFDL Schemas.
    try {
      load(source, Some(XMLUtils.schemaForDFDLSchemas), addPositionAttributes = true)
    } catch {
      // fatal errors are thrown.
      // validation errors are never fatal.
      case e: SAXParseException => {
        // Capturing this would be redundant.
        // It will already have been passed to the errorHandler.fatalError
        // method.
        // So it is explicitly ok to just rethrow this exception.
        // we don't want to record it again, but we do want to stop with a
        // fatal error because the schema was invalid. Daffodil assumes the
        // schema is valid all over its code base.
        throw e
      }
    }
  }

  /**
   * This loads the DFDL schema as an XML Schema. This will
   * check many more things (ex: UPA) about the DFDL schema other than
   * just whether it validates against the XML Schema for DFDL schemas.
   *
   * This should only be called once, for the entire schema, not per file of
   * the schema.
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
  def validateAsCompleteDFDLSchema(source: DaffodilSchemaSource): Boolean = {
    try {
      //
      // We validate explicitly so Xerces can check things
      // such as for UPA violations
      //
      val inputSource = source.newInputSource()
      val saxSource = new SAXSource(inputSource)
      //
      // We would like this saxSource to be created from an XMLReader
      // so that we can call XMLUtils.setSecureDefaults on it.
      // but we get strange errors if I do that, where every symbol
      // in the schema has an unrecognized namespace prefix.
      //
      try {
        schemaFactory.newSchema(saxSource)
      } finally {
        inputSource.getByteStream().close()
      }
      true
    } catch {
      // fatal errors are thrown.
      // validation errors are never fatal.
      case e: SAXParseException => {
        // Capturing this would be redundant.
        // It will already have been passed to the errorHandler.fatalError
        // method.
        // So it is explicitly ok to just rethrow this exception.
        // we don't want to record it again, but we do want to stop with a
        // fatal error because the schema was invalid. Daffodil assumes the
        // schema is valid all over its code base.
        throw e
      }
    }
  }

  // $COVERAGE-OFF$ These three functions should only be used if someone calls one of the
  // Scala-XML load* functions. Only our custom load() functions should be used, which ensures
  // hat correct parses/readers are used
  override def parser = {
    Assert.usageError("not to be called.")
  }
  override def reader = {
    Assert.usageError("not to be called.")
  }
  override def adapter = {
    Assert.usageError("not to be called.")
  }
  // $COVERAGE-ON$

  /**
   * Obtain and initialize parser which validates the schema is defined.
   */
  private def parserFromURI(optSchemaURI: Option[URI]): SAXParser = {
    if (optSchemaURI.isEmpty) noSchemaParser
    else {
      val f = parserFactory()
      val schema = schemaFromURI(optSchemaURI.get)
      f.setSchema(schema)
      parserFromFactory(f)
    }
  }

  private def schemaFromURI(schemaURI: URI): Schema = {
    val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    sf.setErrorHandler(errorHandler)
    sf.setResourceResolver(resolver)
    val schema = sf.newSchema(new StreamSource(schemaURI.toString))
    schema
  }

  private def parserFactory() = {
    val f = DaffodilSAXParserFactory()
    f.setNamespaceAware(true)
    f.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    f.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    f.setValidating(false) // according to javadoc, just controls DTD validation
    f.setFeature("http://xml.org/sax/features/validation", true)
    // not recognized by SAXParserFactory
    // f.setFeature("http://xml.org/sax/features/validation/dynamic", true)
    f.setFeature("http://apache.org/xml/features/honour-all-schemaLocations", true)
    f.setFeature("http://apache.org/xml/features/validation/schema", true)
    f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
    f
  }

  private lazy val noSchemaParser: SAXParser = {
    parserFromFactory(parserFactory())
  }

  private def parserFromFactory(f: SAXParserFactory) = {
    val p = f.newSAXParser()
    // Not allowed on a SAXParser
    // p.setProperty(XMLUtils.SAX_NAvMESPACES_FEATURE, true)
    // Not allowed on a SAXParser
    // p.setProperty(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)
    p
  }

  /**
   * This is the common routine called to actually
   * carry out the loading of the schema.
   *
   * Does (optional) validation if a schema is supplied.
   *
   * @param source The URI for the XML document which may be a XML or DFDL schema, or just XML data.
   * @param optSchemaURI Optional URI for XML schema for the XML source document.
   * @param addPositionAttributes True to add dafint:file dafint:line attributes to all elements.
   *                              Defaults to false.
   * @return an scala.xml.Node (Element actually) which is the document element of the source.
   */
  def load(
    source: DaffodilSchemaSource,
    optSchemaURI: Option[URI],
    addPositionAttributes: Boolean = false
  ): scala.xml.Node =
    load(source, optSchemaURI, addPositionAttributes, normalizeCRLFtoLF = true)

  /**
   * package private constructor gives access to normalizeCRLFtoLF feature.
   *
   * @param source The URI for the XML document which may be a XML or DFDL schema, or just XML data.
   * @param optSchemaURI Optional URI for XML schema for the XML source document.
   * @param addPositionAttributes True to add dafint:file dafint:line attributes to all elements.
   *                              Defaults to false.
   * @param normalizeCRLFtoLF True to normalize CRLF and isolated CR to LF. This should usually be true,
   *                          but some special case situations may require preservation of CRLF/CR.
   * @return an scala.xml.Node (Element actually) which is the document element of the source.
   */
  private[xml] def load(
    source: DaffodilSchemaSource,
    optSchemaURI: Option[URI],
    addPositionAttributes: Boolean,
    normalizeCRLFtoLF: Boolean
  ): scala.xml.Node = {
    //
    // First we invoke the validator to explicitly validate the XML against
    // the XML Schema (not necessarily a DFDL schema), via the
    // javax.xml.validation.Validator's validate method.
    //
    optSchemaURI.foreach { schemaURI =>
      val validator = XercesValidator.fromURIs(Seq(schemaURI))
      val inputStream = source.uriForLoading.toURL.openStream()
      validator.validateXML(inputStream, errorHandler)
      inputStream.close()
      //
      // Next we have to invoke a regular xerces loader, setup for validation
      // because that will actually interpret things like xsi:schemaLocation attributes
      // of the root element.
      //
      // The check of xsi:schemaLocation schemas seems to be the only reason we
      // have to run this additional test.
      //
      // Possibly xsi:noNamespaceSchemaLocation would have the same issue, but as of
      // this writing, we have no tests that use that.
      //
      val parser = parserFromURI(optSchemaURI)
      val xrdr: XMLReader = {
        val r = parser.getXMLReader()

        // We must use XMLReader setProperty() function to set the entity resolver--calling
        // setEntityResolver with the Xerces XML reader causes validation to fail for some
        // reason (we get a "cvc-elt.1.a: Cannot find the declaration of element 'schema'" error).
        // We call the right function below, but unfortunately, scala-xml calls
        // setEntityResolver in loadDocument(), which cannot be disabled and scala-xml does not
        // want to change. To avoid this, we wrap the Xerces XMLReader in an XMLFilterImpl and
        // override setEntityResolver to a no-op. However, XMLFilterImpl parse() calls
        // setEntityResolver() on the XMLReader, which for the same reason as before causes
        // issues. To fix this, we can override parse() to just pass through to the parent, but
        // that means we must override the various set/get handler functions to also pass
        // through to the parent.
        val w = new XMLFilterImpl(r) {
          override def setEntityResolver(resolver: EntityResolver): Unit = {} // no-op
          override def parse(input: InputSource): Unit = getParent.parse(input)

          override def setContentHandler(handler: ContentHandler): Unit =
            getParent.setContentHandler(handler)
          override def setDTDHandler(handler: DTDHandler): Unit =
            getParent.setDTDHandler(handler)
          override def setErrorHandler(handler: ErrorHandler): Unit =
            getParent.setErrorHandler(handler)
          override def getContentHandler(): ContentHandler =
            getParent.getContentHandler()
          override def getDTDHandler(): DTDHandler =
            getParent.getDTDHandler()
          override def getErrorHandler(): ErrorHandler =
            getParent.getErrorHandler()
        }
        w.setErrorHandler(errorHandler)
        w.setProperty("http://apache.org/xml/properties/internal/entity-resolver", resolver)
        w
      }

      val saxSource = scala.xml.Source.fromSysId(source.uriForLoading.toString)
      try {
        // it is important that we call loadDocument to parse/validate the XML instead of
        // directly calling xrdr.parse. Although loadDocument does eventually call xrdr.parse,
        // it first modifies the reader in a number of ways to prepare it for use with this
        // FactoryAdapter, as well as initialize private state that is used by ContentHandler
        // functions.
        loadDocument(saxSource, xrdr)
      } catch {
        // can be thrown by the resolver if a schemaLocation of
        // an import/include cannot be resolved.
        // Regular Xerces doesn't report that as an error.
        case spe: SAXParseException => errorHandler.error(spe)
      }
      // no result, as the errors are reported separately
    }
    //
    // To get reliable xml nodes including conversion of CDATA syntax into
    // PCData nodes, we have to use a different loader.
    //
    val constructingLoader =
      new DaffodilConstructingLoader(
        source.uriForLoading,
        errorHandler,
        addPositionAttributes,
        normalizeCRLFtoLF
      )
    val res =
      try {
        constructingLoader.load() // construct the XML objects for us.
      } catch {
        case e: SAXParseException => // fatal. We can't successfully load.
          throw e // good place for a breakpoint
      } finally {
        constructingLoader.input.close()
      }
    res
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
case class DFDLSchemaValidationWarning(cause: Throwable)
  extends DFDLSchemaValidationException(cause)
case class DFDLSchemaValidationError(cause: Throwable)
  extends DFDLSchemaValidationException(cause)

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
