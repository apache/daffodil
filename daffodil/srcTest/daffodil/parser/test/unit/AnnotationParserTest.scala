package daffodil.parser.test.unit

import scala.collection.mutable.Map

import java.io.StringReader

import org.jdom.input.SAXBuilder

import daffodil.parser.AnnotationParser
import daffodil.schema.annotation.Format
import daffodil.schema.annotation.enumerations._
import daffodil.xml.XMLUtil

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class AnnotationParserTest extends JUnit3Suite {

  // @Test
  def testReadingPropertiesFromAnElement() {
    val builder = new SAXBuilder()
    val xmlNode = XMLUtil.elem2Element(
      <element xmlns="http://www.w3.org/2001/XMLSchema" name="myElement"
      		type="string" maxOccurs="10" minOccurs="12" lengthKind="implicit"/>
      )
    
    val result = AnnotationParser(xmlNode,Map[String,Format]())
    
    assertEquals(Some("http://www.w3.org/2001/XMLSchema/string"), result.format.typeName)
    assertEquals(Some(10), result.format.maxOccurs)
    assertEquals(Some(12), result.format.minOccurs)
    assertEquals(Some(Implicit), result.format.lengthKind)
  }
  
  // @Test
  def testReadingPrefixedPropertiesFromAnElement() {
    val builder = new SAXBuilder()
    val xml = 
      <element xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0"
            name="foobar" type="string" maxOccurs="10" minOccurs="12" dfdl:lengthKind="explicit" dfdl:length="8"/>
      
    val xmlNode = XMLUtil.elem2Element(xml)
            
    val result = AnnotationParser(xmlNode,Map[String,Format]())
    
    assertEquals(Some("http://www.w3.org/2001/XMLSchema/string"), result.format.typeName)
    assertEquals(Some(10), result.format.maxOccurs)
    assertEquals(Some(12), result.format.minOccurs)
    assertEquals(Some(Explicit), result.format.lengthKind)
    assertEquals(Some("8"), result.format.length)      	      	
  }
  
  // test("ignoring properties with wrong prefix") (pending)
  
  // @Test
  def testReadingPropertiesThroughThePropertyTag() {
    val builder = new SAXBuilder()
    val xmlNode = XMLUtil elem2Element(
      <element xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0" type="int" name="Francois">
      	<annotation>
      		<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
      			<dfdl:format>
      				<dfdl:property name="representation">text</dfdl:property>
      			<dfdl:property name="occursCountKind">expression</dfdl:property>
      				<dfdl:property name="occursCount">12</dfdl:property>
      				<dfdl:property name="separator">, . \\n \\r\\n</dfdl:property>
      				<dfdl:property name="textNumberBase">10</dfdl:property>
      			</dfdl:format>
      		</appinfo>
      	</annotation>
      </element>
        )
      
      val map = Map[String,Format]()
      val result = AnnotationParser(xmlNode, map)
    
      assertEquals(Some("http://www.w3.org/2001/XMLSchema/int"), result.format.typeName)
      assertEquals(Some(Text), result.format.representation)
      assertEquals(Some(Expression), result.format.occursCountKind)
      assertEquals(Some("12"), result.format.occursCount)
                                                    
      // assertEquals(Some("(,) (.) (\\n) (\\r\\n)"), result.format.separator.toString) // TODO: fix - dfdl v1 doesn't allow actual regex's for these
      assertTrue(result.format.separator.toString.contains("(.)"))
      assertTrue(result.format.separator.toString.contains("(,)"))
      assertEquals(Some(10), result.format.base)
  }
  
  // @Test
  def testReadingPropertiesFromAnAnnotatedElement() {
    val builder = new SAXBuilder()
    val xmlNode = XMLUtil.elem2Element( 
      <element xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0" type="int" name="Francois">
      	<annotation>
      		<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
      			<dfdl:format representation="text" occursCountKind="expression" occursCount="12"
      			separator=", . \\n \\r\\n" textNumberBase="10"/>
      		</appinfo>
      	</annotation>
      </element>
      )
      
      
      
      val result = AnnotationParser(xmlNode,Map[String,Format]())
    
      assertEquals(Some("http://www.w3.org/2001/XMLSchema/int"), result.format.typeName)
      assertEquals(Some(Text), result.format.representation)
      assertEquals(Some(Expression), result.format.occursCountKind)
      assertEquals(Some("12"), result.format.occursCount) // occursCount is not an int, it's a string (allows expressions)
                                                    
      // assertEquals("(,) (.) (\\n) (\\r\\n)", result.format.separator.toString)
      assertTrue(result.format.separator.toString.contains("(.)"))
      assertTrue(result.format.separator.toString.contains("(,)"))
      assertEquals(Some(10), result.format.base)
  }
                    
  
//  test("ignoring annotations from other sources") (pending)
//  
//  test("reading a format") (pending)
//  
//  test("reading a defineFormat") (pending)
//  
//  test("reading a defineVariable") (pending)
//  
//  test("reading a defineTextNumberFormat") (pending)
//  
//  test("reading all properties") (pending)
//  
//  test("reading refs")(pending)
  
}
