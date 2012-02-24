package daffodil.dsom



import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite
import daffodil.schema.annotation.props.gen._

import scala.xml._

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._



import java.io.File

/**
 * Scala Unit Testing Notes:
 *
 * It is important that the Eclipse IDE make it convenient to run the unit tests, step the user directly to the point
 * of failure, etc.
 *
 * Scalatest doesn't do this directly, but using it driven by JUnit3 does. 
 *
 * So I'm advocating that a much more vanilla approach be taken to unit tests. Straight use of Junit3.
 *
 * Here is an example. Some simple tests, some that intercept exceptions, and demonstrate that the intercept
 * device works properly.
 */
class TestDsomCompiler extends JUnit3Suite {

  // @Test
  def test() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <!-- Basic variable definition and inputValueCalc -->
        <annotation>
          <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
            <dfdl:format byteOrder="bigEndian"/>
          </appinfo>
        </annotation>
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

    val sset = DsomCompiler.compile(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    
    val df = schemaDoc.defaultFormat
    val bo = df.byteOrder
    assertEquals(ByteOrder.BigEndian.toString().toLowerCase(), bo.toLowerCase())
  }
  
   // @Test
  def testSchemaValidationSubset() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence maxOccurs="2"><!-- DFDL SUBSET DOESN'T ALLOW THIS -->
            <element name="w" type="xsd:int"/>
          </sequence>
        </complexType>
      </schema>
   val ex = intercept[Exception] { 
      DsomCompiler.compile(testSchema)
      } 
    // should throw a validation error. 
    println(ex) 
    val msg = ex.getMessage()
    val hasErrorText = msg.contains("maxOccurs");
    assertTrue(hasErrorText)
  } 
  
     // @Test
  def testSchemaValidationPropertyChecking() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:byteOrder="invalidValue" />

          </sequence>
        </complexType>
      </schema>
   val ex = intercept[Exception] { 
      DsomCompiler.compile(testSchema)
      } 
    // should throw a validation error. 
    println(ex) 
    val msg = ex.getMessage()
    val hasErrorText = msg.contains("invalidValue");
    assertTrue(hasErrorText)
  }
  
  def test2() {
      val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <!-- Basic variable definition and inputValueCalc -->
        <element name="list" type="tns:example1">
          <annotation>
            <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
             <dfdl:element encoding="ASCII" alignmentUnits="bytes"/>
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

   val sset = DsomCompiler.compile(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)
        
    val fa = decl.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, fa.alignmentUnits)
    fa.alignmentUnits match {
      case AlignmentUnits.Bits => println("was bits")
      case AlignmentUnits.Bytes => println("was bytes")
    }
  }
  
 /* def testXsomMultifile(){
   
    val parser = new XSOMParser()
    val apf = new DomAnnotationParserFactory()
    parser.setAnnotationParser(apf)

    val inFile = new File("test/first.xsd")

    parser.parse(inFile)

    val sset = parser.getResult()
    val sds = parser.getDocuments().toList
    assertTrue(sds.size() >= 2)
  
    sds.map{sd => println(sd.getSystemId)}
  }*/
  
  def testSequence1(){
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <!-- Basic variable definition and inputValueCalc -->
        <element name="list" type="tns:example1">
          <annotation>
            <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
             <dfdl:element encoding="ASCII" alignmentUnits="bytes"/>
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" maxOccurs="unbounded"/>
          </sequence>
        </complexType>
      </schema>

  val w = Utility.trim(testSchema)
      
   val sset = DsomCompiler.compile(w)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)
    
    val mg = ct.modelGroup.asInstanceOf[Sequence]
    assertTrue(mg.isInstanceOf[Sequence])
    
    val Seq(elem) = mg.children
    assertTrue(elem.isInstanceOf[LocalElementDecl])
    
  } 
  
}