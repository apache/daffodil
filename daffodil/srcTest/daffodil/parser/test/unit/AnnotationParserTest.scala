package daffodil.parser.test.unit

import scala.collection.mutable.Map

import java.io.StringReader

import org.jdom.input.SAXBuilder
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.schema.annotation.Format
import daffodil.schema.annotation.enumerations._

class AnnotationParserTest extends FunSuite with ShouldMatchers {

  test("reading properties from an element") {
    val builder = new SAXBuilder()
    val xmlNode = builder.build(new StringReader( 
      "<element xmlns=\"http://www.w3.org/2001/XMLSchema\" name=\"myElement\""+ 
      "		type=\"string\" maxOccurs=\"10\" minOccurs=\"12\" lengthKind=\"implicit\"/>")) getRootElement
    
    val result = AnnotationParser(xmlNode,Map[String,Format]())
    
    result.format.typeName should equal(Some("string"))
    result.format.maxOccurs should equal(Some(10))
    result.format.minOccurs should equal(Some(12))
    result.format.lengthKind should equal(Some(Implicit))
    
  }
  
  test("reading prefixed properties from an element") {
    val builder = new SAXBuilder()
    val xmlNode = builder.build(new StringReader( 
      "<element xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\""+
      "      type=\"string\" maxOccurs=\"10\" dfdl:minOccurs=\"12\" dfdl:lengthKind=\"explicit\" dfdl:length=\"8\"/>")) getRootElement
            
    val result = AnnotationParser(xmlNode,Map[String,Format]())
    
    result.format.typeName should equal(Some("string"))
    result.format.maxOccurs should equal(Some(10))
    result.format.minOccurs should equal(Some(12))
    result.format.lengthKind should equal(Some(Explicit))
    result.format.length should equal(Some("8"))      	      	
  }
  
  test("ignoring properties with wrong prefix") (pending)
  
  test("reading properties through the 'property' tag") {
    val builder = new SAXBuilder()
    val xmlNode = builder.build(new StringReader( 
      "<element xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" type=\"int\" name=\"Francois\">"+
      "	<annotation>"+
      "		<appinfo source=\"http://www.ogf.org/dfdl/\">"+
      "			<dfdl:format>"+
      "				<dfdl:property name=\"representation\">text</dfdl:property>"+
      "				<dfdl:property name=\"occursCountKind\">expression</dfdl:property>"+
      "				<dfdl:property name=\"occursCount\">12</dfdl:property>"+
      "				<dfdl:property name=\"separator\">, . \\n \\r\\n</dfdl:property>"+
      "				<dfdl:property name=\"textNumberBase\">10</dfdl:property>"+
      "			</dfdl:format>"+
      "		</appinfo>"+
      "	</annotation>"+
      "</element>")) getRootElement
      
      val result = AnnotationParser(xmlNode,Map[String,Format]())
    
      result.format.typeName should equal(Some("int"))
      result.format.representation should equal(Some(Text))
      result.format.occursCountKind should equal(Some(Expression))
      result.format.occursCount should equal(Some("12"))
                                                    
      result.format.separator should equal(Some(", . \\n \\r\\n"))
      result.format.base should equal(Some(10))
  }
  
                                                    
  test("reading properties from an annotated element") {
    val builder = new SAXBuilder()
    val xmlNode = builder.build(new StringReader( 
      "<element xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" type=\"int\" name=\"Francois\">"+
      "	<annotation>"+
      "		<appinfo source=\"http://www.ogf.org/dfdl/\">"+
      "			<dfdl:format representation=\"text\" occursCountKind=\"expression\" occursCount=\"12\""+
      "			separator=\", . \\n \\r\\n\" textNumberBase=\"10\"/>"+
      "		</appinfo>"+
      "	</annotation>"+
      "</element>")) getRootElement
      
      
      
      val result = AnnotationParser(xmlNode,Map[String,Format]())
    
      result.format.typeName should equal(Some("int"))
      result.format.representation should equal(Some(Text))
      result.format.occursCountKind should equal(Some(Expression))
      result.format.occursCount should equal(Some("12"))
                    
      result.format.separator should equal(Some(", . \\n \\r\\n")) 
      result.format.base should equal(Some(10))
  }
                    
  
  test("ignoring annotations from other sources") (pending)
  
  test("reading a format") (pending)
  
  test("reading a defineFormat") (pending)
  
  test("reading a defineVariable") (pending)
  
  test("reading a defineTextNumberFormat") (pending)
  
  test("reading all properties") (pending)
  
  test("reading refs")(pending)
  
}
