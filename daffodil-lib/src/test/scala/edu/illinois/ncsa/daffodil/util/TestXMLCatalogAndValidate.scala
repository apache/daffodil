package edu.illinois.ncsa.daffodil.util

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

import java.io.File
import scala.xml.{ SAXParseException, InputSource }
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml.{ TopScope, Node, Elem }
import org.apache.xerces.dom.DOMInputImpl
import org.apache.xerces.xni.parser.XMLInputSource
import org.apache.xml.resolver.{ CatalogManager, Catalog }
import org.junit.Test
import Implicits.using
import javax.xml.parsers.{ SAXParserFactory, SAXParser }
import junit.framework.Assert.{ fail, assertTrue }
import scala.xml.NamespaceBinding
import scala.xml.MetaData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.xml.Null
import scala.xml.Attribute
import scala.xml.Text
import scala.language.reflectiveCalls

object Implicits {
  /**
   * Used for reading/writing to database, files, etc.
   * Code From the book "Beginning Scala"
   * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
   */
  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }
}

/**
 * The goal here is to integrate XML Catalog with XSD Validation, so that both
 * the DFDL Schema references to other DFDL Schema namespaces would be resolved via
 * the XML Catalog, as well as the DFDL-schema-for-DFDL-schemas, and the TDML test
 * data schemas.
 *
 * Note: the testing technique used in test1 and test2 is something we'll want to copy
 * for testing on the scoping/front-end stuff. It lays down temporary files for schemas
 * and data, and then runs tests, then cleans up the files. This pattern will be
 * prevalent in tests for Daffodil generally.
 *
 * This needs a repeatable framework to avoid all the code duplication.
 */
class TestXMLCatalogAndValidate {

  val tdml = XMLUtil.TDML_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val xsd = XMLUtil.XSD_NAMESPACE
  val example = "http://www.example.com/"
  val ex1 = example + "1"
  val ex2 = example + "2"
  // val sub = XMLUtil.DFDL_XMLSCHEMASUBSET_NAMESPACE

  // In this test, the schema says the data must be an int.
  // The data is 'abc' so this should fail to validate.
  // But that will only happen if we in fact are resolving the URN for example 
  // to a file that it is able to load.
  //
  def tempFileName(suffix: String): File = {
    val f = File.createTempFile("DFDLTest", suffix)
    val fn = f.getAbsolutePath()
    f.delete()
    new File(fn)
  }

  @Test def test1() {

    // lets make sure we're not using files that would naturally be on the classpath
    val tmpSchemaFileName = tempFileName("_sch.xsd")
    val tmpDataFileName = tempFileName("_data.xml")
    val tmpCatalogFileName = tempFileName("_cat.catalog.xml")

    //This version doesn't seem to read a catalog this way
    System.setProperty("xml.catalog.files", tmpCatalogFileName.getAbsolutePath())
    System.setProperty("xml.catalog.ignoreMissing", "false")
    System.setProperty("xml.catalog.verbosity", "4") // has no effect... grr

    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int"/>
      </schema>

    val testSuite = <data xmlns={ example }>abc</data>

    val testCatalog = <catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
                        <uri name={ example } uri={ tmpSchemaFileName.getAbsolutePath }/>
                      </catalog>

    try {
      using(new java.io.FileWriter(tmpSchemaFileName)) {
        fw =>
          fw.write(testSchema.toString())
      }
      using(new java.io.FileWriter(tmpDataFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }
      using(new java.io.FileWriter(tmpCatalogFileName)) {
        fw =>
          fw.write(testCatalog.toString())
      }

      val loader = XMLLoaderFactory()
      val elem = loader.loadFile(tmpDataFileName) // that should validate it.
      val excs = loader.exceptionList

      val hasErroneousData = excs.exists { _.getMessage().contains("abc") }
      assertTrue(hasErroneousData)

    } finally {
      try {
        val f = tmpSchemaFileName
        f.delete()
      } finally {
        try {
          val t = tmpDataFileName
          t.delete()
        } finally {
          val c = tmpCatalogFileName
          c.delete()
        }
      }
    }
  }

  @Test def test2() {

    val tmpSchema1FileName = tempFileName("_sch1.xsd")
    val tmpSchema2FileName = tempFileName("_sch2.xsd")
    val tmpDataFileName = tempFileName("_data.xml")
    val tmpCatalogFileName = tempFileName("_cat.catalog.xml")

    System.setProperty("xml.catalog.files", tmpCatalogFileName.getAbsolutePath)
    System.setProperty("xml.catalog.ignoreMissing", "no")
    System.setProperty("xml.catalog.verbosity", "4") // has no effect... grr

    val testSchema1 =
      <schema xmlns={ xsd } targetNamespace={ ex1 } xmlns:ex2={ ex2 }>
        <import namespace={ ex2 }/>
        <element name="data" type="ex2:dataType"/>
      </schema>
    val testSchema2 =
      <schema xmlns={ xsd } targetNamespace={ ex2 } xmlns:xsd={ xsd }>
        <simpleType name="dataType">
          <restriction base="xsd:int"/>
        </simpleType>
      </schema>
    System.setProperty("xml.catalog.files", tmpCatalogFileName.getAbsolutePath())

    val testSuite = <data xmlns={ ex1 }>abc</data>

    val testCatalog = <catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
                        <uri name={ ex1 } uri={ tmpSchema1FileName.getAbsolutePath() }/>
                        <uri name={ ex2 } uri={ tmpSchema2FileName.getAbsolutePath() }/>
                      </catalog>

    try {
      using(new java.io.FileWriter(tmpSchema1FileName)) {
        fw =>
          fw.write(testSchema1.toString())
      }
      using(new java.io.FileWriter(tmpSchema2FileName)) {
        fw =>
          fw.write(testSchema2.toString())
      }
      using(new java.io.FileWriter(tmpDataFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }
      using(new java.io.FileWriter(tmpCatalogFileName)) {
        fw =>
          fw.write(testCatalog.toString())
      }

      val loader = XMLLoaderFactory()
      val elem = loader.loadFile(tmpDataFileName) // that should validate it.
      val excs = loader.exceptionList

      val hasErroneousData = excs.exists { _.getMessage().contains("abc") }
      assertTrue(hasErroneousData)

    } finally {
      try {
        val f = tmpSchema1FileName
        f.delete()
      } finally {
        try {
          val f = tmpSchema2FileName
          f.delete()
        } finally {
          try {
            val t = tmpDataFileName
            t.delete()
          } finally {
            val c = tmpCatalogFileName
            c.delete()
          }
        }
      }
    }
  }

  @Test def test3() {
    // lets make sure we're not using files that would naturally be on the classpath
    System.setProperty("xml.catalog.ignoreMissing", "true")

    val testSuite = <schema xmlns={ xsd } targetNamespace={ ex1 }>
                      <foobar/>
                    </schema>

    val tmpDataFileName = tempFileName("_data.xml")
    try {
      using(new java.io.FileWriter(tmpDataFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }

      val loader = XMLLoaderFactory()
      val elem = loader.loadFile(tmpDataFileName) // that should validate it.
      val excs = loader.exceptionList

      val hasErroneousData = excs.exists { _.getMessage().contains("foobar") }
      assertTrue(hasErroneousData)

    } finally {
      val t = tmpDataFileName
      t.delete()
    }
  }

  @Test def test4() {
    // lets make sure we're not using files that would naturally be on the classpath
    System.setProperty("xml.catalog.ignoreMissing", "false")

    val testSuite = <schema xmlns={ xsd } targetNamespace={ ex1 } xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
                      <annotation><appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                                    <dfdl:format foobar="quuxly"/>
                                  </appinfo></annotation>
                    </schema>

    val tmpDataFileName = tempFileName("_data.xml")
    try {
      using(new java.io.FileWriter(tmpDataFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }

      val loader = XMLLoaderFactory()
      val elem = loader.loadFile(tmpDataFileName) // that should validate it.
      val excs = loader.exceptionList
      // println("Element = " + elem)
      // println("Exception List = " + excs)
      val hasErroneousData = excs.exists { _.getMessage().contains("foobar") }
      assertTrue(hasErroneousData)

    } finally {
      val t = tmpDataFileName
      t.delete()
    }
  }
}

// From: http://weblogs.java.net/blog/cayhorstmann/archive/2011/12/12/sordid-tale-xml-catalogs

object XMLLoaderFactory {
  def apply() = {
    val loader = new SchemaAwareFactoryAdapter()
    //    val loader = new factory.XMLLoader[Elem] {
    //      override def adapter = new SchemaAwareFactoryAdapter // new parsing.NoBindingFactoryAdapter() {
    //    }
    loader
  }

  //  val doc = loader.load(new URL("http://horstmann.com/index.html"))
  //  println(doc);
}

// From http://stackoverflow.com/questions/1627111/how-does-one-validate-the-schema-of-an-xml-file-using-scala

class SchemaAwareFactoryAdapter()
  extends NoBindingFactoryAdapter {

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
    val nsURI = scope.getURI(pre)
    val isXSSchemaNode = (label == "schema" && nsURI != null &&
      (nsURI == XMLUtil.XSD_NAMESPACE))
    val isTDMLTestSuiteNode = (label == "testSuite" && nsURI != null &&
      nsURI == XMLUtil.TDML_NAMESPACE)
    val isFileRootNode = isXSSchemaNode || isTDMLTestSuiteNode

    // augment the scope with the dafint namespace binding but only
    // for root nodes (to avoid clutter with the long xmlns:dafint="big uri")
    // and only if it isn't already there.
    val scopeWithDafInt =
      if (scope.getPrefix(XMLUtil.INT_NS) == null
        && isFileRootNode)
        NamespaceBinding(XMLUtil.INT_PREFIX, XMLUtil.INT_NS, scope)
      else scope

    val haveFileName = isFileRootNode && fileName != ""

    val asIs = super.createNode(pre, label, attrs, scopeWithDafInt, children)

    val alreadyHasFile = attrs.get(XMLUtil.INT_NS, scopeWithDafInt, XMLUtil.FILE_ATTRIBUTE_NAME) != None

    // If there is already a _line_ attribute, then we're reloading something
    // that was probably converted back into a string and written out. 
    // The original line numbers are therefore the ones wanted, not any new
    // line numbers, so we don't displace any line numbers that already existed.

    val alreadyHasLine = attrs.get(XMLUtil.INT_NS, scopeWithDafInt, XMLUtil.LINE_ATTRIBUTE_NAME) != None
    val alreadyHasCol = attrs.get(XMLUtil.INT_NS, scopeWithDafInt, XMLUtil.COLUMN_ATTRIBUTE_NAME) != None
    Assert.invariant(alreadyHasLine == alreadyHasCol)

    val lineAttr =
      if (alreadyHasLine) Null
      else Attribute(XMLUtil.INT_PREFIX, XMLUtil.LINE_ATTRIBUTE_NAME, Text(elementStartLocator.line.toString), Null)
    val colAttr =
      if (alreadyHasCol) Null
      else Attribute(XMLUtil.INT_PREFIX, XMLUtil.COLUMN_ATTRIBUTE_NAME, Text(elementStartLocator.col.toString), Null)
    val fileAttr =
      if (alreadyHasFile || !haveFileName) Null
      else {
        Attribute(XMLUtil.INT_PREFIX, XMLUtil.FILE_ATTRIBUTE_NAME, Text(fileName), Null)
      }

    // % operator creates a new element with updated attributes
    val res = asIs % lineAttr % colAttr % fileAttr
    //        % (if (hasSysID) Attribute("_sysID_", Text(elementStartLocator.sysID), Null) else Null)
    //        % (if (hasPubID) Attribute("_pubID_", Text(elementStartLocator.pubID), Null) else Null)
    // System.err.println("Create Node: " + res)
    res
  }

  override def startElement(uri: String, _localName: String, qname: String, attributes: org.xml.sax.Attributes): Unit = {
    // System.err.println("startElement")
    val loc = Locator(saxLocator.getLineNumber, saxLocator.getColumnNumber, saxLocator.getSystemId, saxLocator.getPublicId)
    locatorStack.push(loc)
    super.startElement(uri, _localName, qname, attributes)
  }

  override def endElement(uri: String, _localName: String, qname: String): Unit = {
    // println("endElement")
    elementStartLocator = locatorStack.pop
    super.endElement(uri, _localName, qname)
  }

  //System.err.println("Creating " + getClass().getName())
  val res = new MyResolver()
  //System.err.println("Creating parser")
  val f = SAXParserFactory.newInstance()
  f.setNamespaceAware(true)
  f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
  f.setFeature("http://xml.org/sax/features/validation", true)
  f.setFeature("http://apache.org/xml/features/validation/dynamic", true)
  f.setFeature("http://apache.org/xml/features/validation/schema", true)
  f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
  f.setValidating(true)
  val p = f.newSAXParser()
  val xr = p.getXMLReader()
  xr.setContentHandler(this)
  xr.setEntityResolver(res) // older API??
  xr.setProperty(
    "http://apache.org/xml/properties/internal/entity-resolver",
    res)
  //

  override def parser: SAXParser = p

  var exceptionList: List[Exception] = Nil

  override def loadXML(source: InputSource, ignored: SAXParser): Node = {
    //System.err.println("loadXML")
    val xr = parser.getXMLReader()
    xr.setErrorHandler(new org.xml.sax.ErrorHandler() {

      def warning(exception: SAXParseException) = {
        System.err.println(exception.getMessage())
        exceptionList :+= exception
      }

      def error(exception: SAXParseException) = {
        System.err.println("Error: " + exception.getMessage())
        exceptionList :+= exception
      }
      def fatalError(exception: SAXParseException) = {
        System.err.println(exception.getMessage())
        exceptionList :+= exception
      }
    })

    //    val schemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
    //    //System.err.println("Reading the schema")
    //    schemaFactory.setResourceResolver(res)
    //    val schema = schemaFactory.newSchema(schemaFile);
    //    //System.err.println("Done reading the schema")
    //    // What if, instead of this, we just setFeature(...validation...) above?
    //    // Answer: it doesn't throw exceptions on validation errors.
    //    val vh = schema.newValidatorHandler()
    //    vh.setContentHandler(this)
    //    xr.setContentHandler(vh)

    // parse file
    scopeStack.push(TopScope)
    //System.err.println("beginning parse")
    xr.parse(source)
    //System.err.println("ending parse")
    scopeStack.pop
    return rootElem.asInstanceOf[Elem]
  }
}

/**
 * There are no trace/verbose options (that work) in XMLCatalogResolver
 * so this delegating class is created so that we have a place to put
 * breakpoints, and logging/debug stuff having to do with resolution of
 * uris/urns to files.
 */
class MyResolver()
  extends org.apache.xerces.xni.parser.XMLEntityResolver
  with org.w3c.dom.ls.LSResourceResolver
  with org.xml.sax.EntityResolver
  with org.xml.sax.ext.EntityResolver2 {
  val cm = new CatalogManager()
  val catFiles = cm.getCatalogFiles().toArray.toList.asInstanceOf[List[String]]
  // println("catalog files: " + catFiles)
  cm.setIgnoreMissingProperties(false)
  cm.setRelativeCatalogs(true)
  cm.setVerbosity(4)
  cm.debug.setDebug(100)
  val delegate = // new org.apache.xerces.util.XMLCatalogResolver() // cl)
    new Catalog(cm) {
      catalogManager.debug.setDebug(100)
    }
  delegate.setupReaders()
  delegate.loadSystemCatalogs()
  // delegate.parseAllCatalogs()
  delegate.parseCatalog(Misc.getRequiredResource("/daffodil-built-in-catalog.xml"))
  // catFiles.foreach { cf => delegate.parseCatalog(Misc.getRequiredResource(cf)) }

  def resolveEntity(ri: org.apache.xerces.xni.XMLResourceIdentifier) = {
    // println("resolveEntity1")
    val resolvedId = delegate.resolveURI(ri.getNamespace())
    val res = if (resolvedId != null) {
      new XMLInputSource(ri.getPublicId(),
        resolvedId,
        ri.getBaseSystemId())
    } else null
    // println("resolved to " + resolvedId)
    res
  }

  def resolveResource(type_ : String, nsURI: String, publicId: String, systemId: String, baseURI: String) = {
    //    println("resolveResource nsURI = %s, baseURI = %s".format(nsURI, baseURI))
    //    val resource = delegate.resolveURI(nsURI) // (type_, nsURI, publicId, systemId, baseURI)
    //    println("resolveResource = " + resource)
    //    val res = new DOMInputImpl(publicId, resource, baseURI);
    //    res
    fail()
    null
  }

  def resolveEntity(publicId: String, systemID: String) = {
    println("resolveEntity2")
    fail()
    null
  }

  def getExternalSubset(name: String, baseURI: String) = {
    println("getExternalSubset name = %s, baseURI = %s".format(name, baseURI))
    // delegate.getExternalSubset(name, baseURI)
    null
  }

  def resolveEntity(name: String, publicId: String, baseURI: String, systemID: String) = {
    println("resolveEntity3")
    fail()
    null
  }
}

/**
 * We're in daffodil-lib. Until we move XMLUtil here, we need this.
 */
object XMLUtil {
  private val DAFFODIL_EXTENSIONS_NAMESPACE_ROOT = "urn:ogf:dfdl:2013:imp:opensource.ncsa.illinois.edu:2012" // TODO: finalize syntax of this URN
  val DAFFODIL_EXTENSION_NAMESPACE = DAFFODIL_EXTENSIONS_NAMESPACE_ROOT + ":ext"
  val DAFFODIL_INTERNAL_NAMESPACE = DAFFODIL_EXTENSIONS_NAMESPACE_ROOT + ":int"
  val EXT_NS = DAFFODIL_EXTENSION_NAMESPACE
  val EXT_PREFIX = "daf"
  val EXT_NS_OBJECT = org.jdom.Namespace.getNamespace(EXT_PREFIX, EXT_NS)
  val INT_NS = DAFFODIL_INTERNAL_NAMESPACE
  val INT_PREFIX = "dafint"
  val INT_NS_OBJECT = org.jdom.Namespace.getNamespace(INT_PREFIX, INT_NS)

  val FILE_ATTRIBUTE_NAME = "file"
  val LINE_ATTRIBUTE_NAME = "line"
  val COLUMN_ATTRIBUTE_NAME = "col"
  val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema" // removed trailing slash (namespaces care)
  val XSI_NAMESPACE = "http://www.w3.org/2001/XMLSchema-instance"
  val DFDL_NAMESPACE = "http://www.ogf.org/dfdl/dfdl-1.0/" // dfdl ns does have a trailing slash
  val TDML_NAMESPACE = "http://www.ibm.com/xmlns/dfdl/testData"
  // val DFDL_XMLSCHEMASUBSET_NAMESPACE = "http://www.w3.org/2001/XMLSchema"
  val EXAMPLE_NAMESPACE = "http://example.com"
  // val DFDL_SUBSET_NAMESPACE = "http://www.w3.org/2001/XMLSchema"
}

