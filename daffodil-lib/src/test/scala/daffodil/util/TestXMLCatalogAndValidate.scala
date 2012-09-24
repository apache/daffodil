//package daffodil.util
//
//import scala.xml._
//import junit.framework.Assert._;
//import org.scalatest.junit.JUnitSuite;
//import java.io.StringReader;
//import java.io.InputStreamReader;
//import java.io.File
//import xml._
//import java.net._
//import javax.xml.validation.SchemaFactory
//
//import javax.xml.parsers.SAXParser
//import javax.xml.parsers.SAXParserFactory
//import javax.xml.validation.Schema
//import javax.xml.validation.ValidatorHandler
//import org.xml.sax.XMLReader
//import scala.xml.parsing.NoBindingFactoryAdapter
//import com.sun.org.apache.xerces.internal.xni.XMLResourceIdentifier

//object Implicits {
//  /**
//   * Used for reading/writing to database, files, etc.
//   * Code From the book "Beginning Scala"
//   * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
//   */
//  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
//    try { f(param) } finally { param.close() }
//}
//import Implicits._

/**
 * These tests have convinced me (Mike Beckerle) that as of
 * this writing (2012-02-28) XML Catalogs are not yet
 * ready for prime time. There are bugs and/or APIs that are too fragile or too
 * hard to access, or that don't interoperate with other APIs we need. (Validation)
 * 
 * (Unfortunately, this means we implement our own simplified moral equivalent
 * of XML catalogs.)
 * 
 * These can all be commented out so that they aren't running anymore, but it's 
 * worth keeping them in case (a) somebody can fix them and convince me/us that
 * this stuff is worth using (b) to document our efforts on XML Catalog.
 * 
 * The test1 runs, test2 shows the problem, which is that the catalog isn't being used to 
 * resolve the names. It can't find the schema because we put it down one directory level
 * into the lib dir. It works if you put the schema right next to the catalog and
 * other schemas, but move one of them elsewhere and depend on the catalog to 
 * indicate where it is, and it fails.
 * 
 * The goal here was to integrate XML Catalog with XSD Validation, so that both 
 * the DFDL Schema references to other DFDL Schema namespaces would be resolved via
 * the XML Catalog, as well as the DFDL-schema-for-DFDL-schemas, and the TDML test
 * data schemas.
 * 
 * Note: the testing technique used in test1 and test2 is something we'll want to copy 
 * for testing on the scoping/front-end stuff. It lays down temporary files for schemas
 * and data, and then runs tests, then cleans up the files. This pattern will be 
 * prevalent in tests for Daffodil generally.
 */
//class TestXMLCatalogAndValidate extends JUnitSuite {
//
//  val tdml = XMLUtil.TDML_NAMESPACE
//  val dfdl = XMLUtil.DFDL_NAMESPACE
//  val xsi = XMLUtil.XSI_NAMESPACE
//  val xsd = XMLUtil.XSD_NAMESPACE
//  val example = "-//Example"
//  val ex1 = example + "1"
//  val ex2 = example + "2"
//  val sub = XMLUtil.DFDL_XMLSCHEMASUBSET_NAMESPACE
//
//  @Test def test1() {
//    val testSchema =
//      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
//        <element name="data" type="xsd:int"/>
//      </schema>
//    val tmpSchemaFileName = getClass.getName() + ".xsd"
//    val tmpDataFileName = getClass.getName() + ".xml"
//    val tmpCatalogFileName = getClass.getName() + ".catalog.xml"
//    System.setProperty("xml.catalog.files", tmpCatalogFileName)
//
//    // xsi:schemaLocation={ example } xmlns:xsi={ xsi }
//    val testSuite = <data xmlns={ example }>abc</data>
//
//    val testCatalog = <catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public">
//                        <uri name={ example } uri={ tmpSchemaFileName }/>
//                      </catalog>
//
//    try {
//      using(new java.io.FileWriter(tmpSchemaFileName)) {
//        fw =>
//          fw.write(testSchema.toString())
//      }
//      using(new java.io.FileWriter(tmpDataFileName)) {
//        fw =>
//          fw.write(testSuite.toString())
//      }
//      using(new java.io.FileWriter(tmpCatalogFileName)) {
//        fw =>
//          fw.write(testCatalog.toString())
//      }
//
//      val cl = List(tmpCatalogFileName).toArray
//      val loader = XMLLoaderFactory(cl, new File(tmpSchemaFileName))
//      val exc = intercept[Exception] {
//        val elem = loader.loadFile(tmpDataFileName) // that should validate it.
//        System.err.println(elem)
//      }
//      val hasErroneousData = exc.getMessage().contains("abc")
//      assertTrue(hasErroneousData)
//
//    } finally {
//      try {
//        val f = new java.io.File(tmpSchemaFileName)
//        f.delete()
//      } finally {
//        try {
//          val t = new java.io.File(tmpDataFileName)
//          t.delete()
//        } finally {
//          val c = new java.io.File(tmpCatalogFileName)
//          c.delete()
//        }
//      }
//    }
//  }
//
//  @Test def test2() {
//    val tmpSchema1FileName = getClass.getName() + "1.xsd"
//    val tmpSchema2FileNamePart = getClass.getName() + "2.xsd"
//    val tmpSchema2FileName = "lib/" + tmpSchema2FileNamePart
//    val testSchema1 =
//      <schema xmlns={ xsd } targetNamespace={ ex1 } xmlns:ex2={ ex2 } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
//        <import namespace={ ex2 } schemaLocation={ tmpSchema2FileNamePart }/>
//        <element name="data" type="ex2:dataType"/>
//      </schema>
//    val testSchema2 =
//      <schema xmlns={ xsd } targetNamespace={ ex2 } xmlns:tns={ ex2 } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
//        <simpleType name="dataType">
//            <restriction base="xsd:int"/>
//        </simpleType>
//      </schema>
//
//    val tmpDataFileName = getClass.getName() + ".xml"
//    val tmpCatalogFileName = getClass.getName() + ".catalog.xml"
//    System.setProperty("xml.catalog.files", tmpCatalogFileName)
//
//    // xsi:schemaLocation={ example } xmlns:xsi={ xsi }
//    val testSuite = <data xmlns={ ex1 }>abc</data>
//
//    val testCatalog = <catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public">
//                        <uri name={ ex1 } uri={ tmpSchema1FileName }/>
//                        <uri name={ ex2 } uri={ tmpSchema2FileName }/>
//                      </catalog>
//
//    try {
//      using(new java.io.FileWriter(tmpSchema1FileName)) {
//        fw =>
//          fw.write(testSchema1.toString())
//      }
//      using(new java.io.FileWriter(tmpSchema2FileName)) {
//        fw =>
//          fw.write(testSchema2.toString())
//      }
//      using(new java.io.FileWriter(tmpDataFileName)) {
//        fw =>
//          fw.write(testSuite.toString())
//      }
//      using(new java.io.FileWriter(tmpCatalogFileName)) {
//        fw =>
//          fw.write(testCatalog.toString())
//      }
//
//      val cl = List(tmpCatalogFileName).toArray
//      val loader = XMLLoaderFactory(cl, new File(tmpSchema1FileName))
//      val exc = intercept[Exception] {
//        val elem = loader.loadFile(tmpDataFileName) // that should validate it.
//        System.err.println(elem)
//      }
//      val hasErroneousData = exc.getMessage().contains("abc")
//      if (hasErroneousData) assertTrue(hasErroneousData)
//      else throw exc
//
//    } finally {
//      try {
//        val f = new java.io.File(tmpSchema1FileName)
//        f.delete()
//      } finally {
//        try {
//          val f = new java.io.File(tmpSchema2FileName)
//          f.delete()
//        } finally {
//          try {
//            val t = new java.io.File(tmpDataFileName)
//            t.delete()
//          } finally {
//            val c = new java.io.File(tmpCatalogFileName)
//            c.delete()
//          }
//        }
//      }
//    }
//  }
//}

//class ErrorHandler extends org.xml.sax.ErrorHandler {
//
//  def warning(exception: SAXParseException) = {
//    System.err.print(exception.getMessage())
//  }
//
//  def error(exception: SAXParseException) = {
//    System.err.print(exception.getMessage())
//  }
//  def fatalError(exception: SAXParseException) = {
//    System.err.print(exception.getMessage())
//  }
//
//}
//
//// From: http://weblogs.java.net/blog/cayhorstmann/archive/2011/12/12/sordid-tale-xml-catalogs
//
//object XMLLoaderFactory {
//  def apply(catalogList: Array[String], schema: File) = {
//    val loader = new SchemaAwareFactoryAdapter(catalogList, schema)
//    //    val loader = new factory.XMLLoader[Elem] {
//    //      override def adapter = new SchemaAwareFactoryAdapter // new parsing.NoBindingFactoryAdapter() {
//    //    }
//    loader
//  }
//
//  //  val doc = loader.load(new URL("http://horstmann.com/index.html"))
//  //  println(doc);
//}
//
//// From http://stackoverflow.com/questions/1627111/how-does-one-validate-the-schema-of-an-xml-file-using-scala
//
//class SchemaAwareFactoryAdapter(catalogList: Array[String], schemaFile: File) // (schema:Schema) // don't take schema. Use file-specified schemas
//  extends NoBindingFactoryAdapter
//  // with com.sun.org.apache.xerces.internal.xni.parser.XMLEntityResolver
//  with org.apache.xerces.xni.parser.XMLEntityResolver
//  with org.xml.sax.ext.EntityResolver2 {
//
//  val dh = new org.xml.sax.ext.DefaultHandler2
//  def getExternalSubset(x: String, y: String) = dh.getExternalSubset(x, y)
//
//  override def adapter = this
//
//  System.setProperty("xml.catalog.verbosity", "4") // has no effect... grr
//
//  System.err.println("Creating " + getClass().getName())
//  val res = new org.apache.xerces.util.XMLCatalogResolver(catalogList) // new com.sun.org.apache.xml.internal.resolver.tools.CatalogResolver  // new org.apache.xml.resolver.tools.CatalogResolver 
//  // res.setCatalogList(catalogList)
//
//  override def resolveEntity(name: String, publicId: String, baseURI: String, systemId: String): InputSource = {
//    System.out.println("org.xml.sax.ext.EntityResolver2.resolveEntity4: ns = " + name + " publicId = " + publicId + ", systemId = " + systemId + " baseURI = " + baseURI)
//    val result = res.resolveEntity(name, publicId, baseURI, systemId)
//    result
//  }
//
//  override def resolveEntity(publicId: String, systemId: String) = {
//    System.out.println("resolveEntity2: publicId = " + publicId + ", systemId = " + systemId)
//    res.resolveEntity(publicId, systemId)
//  }
//
//  def resolveEntity(ri: org.apache.xerces.xni.XMLResourceIdentifier): org.apache.xerces.xni.parser.XMLInputSource = {
//    val ns = ri.getNamespace()
//    val base = ri.getBaseSystemId()
//    val publicId = ri.getPublicId()
//    val systemId = ri.getLiteralSystemId()
//    System.out.println("org.apache.xerces.xni.parser.XMLEntityResolver.resolveEntity1: ns = " + ns + " publicId = " + publicId + ", systemId = " + systemId + " baseSystemId = " + base)
//    // val inputSource = res.resolveEntity(publicId, systemId)
//    val inputSource = res.resolveEntity(ri) // .resolveEntity(ns, systemId)
//    System.err.println("resolved to: " + inputSource)
//    //    val xmlInputSource = new org.apache.xerces.xni.parser.XMLInputSource(ri)
//    //    xmlInputSource.setByteStream(inputSource.getByteStream())
//    //    xmlInputSource
//    val cl = res.getCatalogList().toList
//    System.err.println("Catalogs: " + cl)
//    inputSource
//  }

  //  def resolveEntity(ri : XMLResourceIdentifier) = {
  //    val ns = ri.getNamespace()
  //    val base = ri.getBaseSystemId()
  //    val publicId = ri.getPublicId()
  //    val systemId = ri.getLiteralSystemId()
  //    System.out.println("com.sun.org.apache.xerces.internal.xni.parser.XMLEntityResolver.resolveEntity1: ns = " + ns + " publicId = " + publicId + ", systemId = " + systemId + " baseSystemId = " + base)
  //    // val inputSource = res.resolveEntity(publicId, systemId)
  //    val inputSource = res.resolveEntity(ns, systemId)
  //    val xmlInputSource = new com.sun.org.apache.xerces.internal.xni.parser.XMLInputSource(ri)
  //    xmlInputSource.setByteStream(inputSource.getByteStream())
  //    xmlInputSource
  //  }

  //  override def resolveEntity(name : String, publicId : String, baseURI : String, systemId : String) = {
  //    System.out.println("resolveEntity4")
  //    res.resolveEntity(publicId, systemId)
  //  }

//  override def parser: SAXParser = parserVal
//
//  lazy val parserVal = try {
//    System.err.println("Creating parser")
//    val f = SAXParserFactory.newInstance()
//    f.setNamespaceAware(true)
//    f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
//    //f.setFeature("http://xml.org/sax/features/validation", true)
//    // f.setFeature("http://apache.org/xml/features/validation/dynamic", true)
//    f.setFeature("http://apache.org/xml/features/validation/schema", true)
//    f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
//    f.setValidating(true)
//    val p = f.newSAXParser()
//    val xr = p.getXMLReader()
//    xr.setEntityResolver(this)
//    xr.setProperty(
//      "http://apache.org/xml/properties/internal/entity-resolver",
//      this)
//    p
//  } catch {
//    case e: Exception =>
//      Console.err.println("error: Unable to instantiate parser")
//      throw e
//  }
//
//  override def loadXML(source: InputSource, ignored: SAXParser): Node = {
//    System.err.println("loadXML")
//    val xr = parser.getXMLReader()
//    xr.setErrorHandler(new ErrorHandler()) // shut up warning.
//    val id = "http://xml.org/sax/features/validation";
//    if (xr.getFeature(id)) {
//      System.err.println("Parser is validating.");
//    } else {
//      System.err.println("Parser is not validating.");
//    }
//    val schemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
//    val schema = schemaFactory.newSchema(schemaFile);
//    //    super.loadXML(source, parser)
//    //    // ignore incoming parser, create parser and set to do validation.
//    //        val parser: SAXParser = try {
//    //          val f = SAXParserFactory.newInstance()
//    //          f.setNamespaceAware(true)
//    //          f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
//    //          f.setFeature("http://xml.org/sax/features/validation", true)
//    //          // f.setFeature("http://apache.org/xml/features/validation/dynamic", true)
//    //          f.setFeature("http://apache.org/xml/features/validation/schema", true)
//    //          f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
//    //          f.newSAXParser()
//    //        } catch {
//    //          case e: Exception =>
//    //            Console.err.println("error: Unable to instantiate parser")
//    //            throw e
//    //        }
//
//    // What if, instead of this, we just setFeature(...validation...) above
//    val vh = schema.newValidatorHandler()
//    vh.setContentHandler(this)
//    xr.setContentHandler(vh)
//
//    // parse file
//    scopeStack.push(TopScope)
//    xr.parse(source)
//    scopeStack.pop
//    return rootElem.asInstanceOf[Elem]
//  }
//}
//
///**
// * We're in daffodil-lib. Until we move XMLUtil here, we need this.
// */
//object XMLUtil {
//  val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema" // removed trailing slash (namespaces care)
//  val XSI_NAMESPACE = "http://www.w3.org/2001/XMLSchema-instance"
//  val DFDL_NAMESPACE = "http://www.ogf.org/dfdl/dfdl-1.0/" // dfdl ns does have a trailing slash
//  val TDML_NAMESPACE = "http://www.ibm.com/xmlns/dfdl/testData"
//  val DFDL_XMLSCHEMASUBSET_NAMESPACE = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset"
//  val EXAMPLE_NAMESPACE = "http://example.com"
//}

