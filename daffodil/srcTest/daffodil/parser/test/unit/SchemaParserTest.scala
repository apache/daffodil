package daffodil.parser.test.unit

import java.io.ByteArrayInputStream
import java.io.StringReader
import scala.xml._
import org.jdom.Element
import org.jdom.input.SAXBuilder
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import daffodil.parser.RollbackStream
import daffodil.parser.SchemaParser
import daffodil.processors.VariableMap
import daffodil.schema._
import daffodil.schema.SimpleElement
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.enumerations.Text
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil
import daffodil.Implicits._

import org.scalatest.junit.JUnit3Suite
import scala.collection.mutable.ListBuffer
import junit.framework.Assert._

class SchemaParserTest extends JUnit3Suite {

//  test("parse simple element") (pending)
  
//  test("parse simple extended element") (pending)
  
//  test("parse complexType/choice") (pending)
  
  // @Test
  def testParseComplexTypeSequence() {
    val xmlNode = 
      <complexType name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				<dfdl:format representation="text" encoding="UTF-8" separator="," />
			</appinfo>
		</annotation>
		<sequence>
			<element name="w" type="int"/>
			<element name="x" type="int"/>
			<element name="y" type="double"/>
			<element name="z" type="float"/>
		</sequence>
	</complexType>;
 
    val parser = new SchemaParser()
    val result = parser.parseComplexType(XMLUtil elem2Element(xmlNode))
 
    val a0 = new Annotation(null)
    a0.format setRepresentation(Text)
    a0.format setEncoding("UTF-8")
    a0.format setSeparator(",")
    
    val a1 = new Annotation(null); a1.format setTypeName(XMLUtil.XSD_INT)
    val a2 = new Annotation(null); a2.format setTypeName(XMLUtil.XSD_INT)
    val a3 = new Annotation(null); a3.format setTypeName(XMLUtil.XSD_DOUBLE)
    val a4 = new Annotation(null); a4.format setTypeName(XMLUtil.XSD_FLOAT)
    
    val expectedResult = new Sequence(a0,null,new Namespaces,
    List(new SimpleElement("w",a1,null,new Namespaces),
         new SimpleElement("x",a2,null,new Namespaces),
         new SimpleElement("y",a3,null,new Namespaces),
         new SimpleElement("z",a4,null,new Namespaces)))
    val diff = expectedResult.diff(result)
    assertEquals(Same, diff)
  }
  
  // @Test
  def testParseComplexTypeSequence2() {
    val xmlNode = 
      <complexType name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				<dfdl:format representation="text" encoding="UTF-16" separator="," numberDecimalSeparator="##"/>
			</appinfo>
		</annotation>
		<sequence>
			<element name="w" type="int"/>
			<element name="x" type="int"/>
			<element name="y" type="double"/>
			<element name="z" type="float"/>
		</sequence>
	</complexType>;
 
    val parser = new SchemaParser().parseComplexType(XMLUtil elem2Element(xmlNode))
    
    
    val a0 = new Annotation(null)
    a0.format setRepresentation(Text)
    a0.format setEncoding("UTF-16")
    a0.format setSeparator(",")
    a0.format setDecimalSeparator("##")
    
    assertTrue(parser.isInstanceOf[Sequence])
    assertEquals(a0, parser.annotation)
        
    val children = parser.asInstanceOf[Sequence].getChildren
    assertTrue(children(0).isInstanceOf[SimpleElement])
    assertTrue(children(1).isInstanceOf[SimpleElement])
    assertTrue(children(2).isInstanceOf[SimpleElement])
    assertTrue(children(3).isInstanceOf[SimpleElement])
    
    assertEquals("w", children(0).asInstanceOf[SimpleElement].name)
    assertEquals("x", children(1).asInstanceOf[SimpleElement].name)
    assertEquals("y", children(2).asInstanceOf[SimpleElement].name)
    assertEquals("z", children(3).asInstanceOf[SimpleElement].name)
    
    val a1 = new Annotation(null); a1.format setTypeName(XMLUtil.XSD_INT)
    val a2 = new Annotation(null); a2.format setTypeName(XMLUtil.XSD_INT)
    val a3 = new Annotation(null); a3.format setTypeName(XMLUtil.XSD_DOUBLE)
    val a4 = new Annotation(null); a4.format setTypeName(XMLUtil.XSD_FLOAT)
    
    assertEquals(a1, children(0).annotation)
    assertEquals(a2, children(1).annotation)
    assertEquals(a3, children(2).annotation)
    assertEquals(a4, children(3).annotation)   
  }
  
  // @Test
  def testParseSimpleInt()
  {
    val xmlNode = 
      <element name="example1" type="int" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				<dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" textNumberBase="10"/>
			</appinfo>
		</annotation>
	</element>;
 
	val data="19"
	val dataStream = new RollbackStream(new ByteArrayInputStream(data.getBytes()))
 
	val parser = new SchemaParser().parseElementNode(XMLUtil elem2Element(xmlNode), Nil)
     
	val expectedResult = List(XMLUtil.element("example1","19"))

    val annotation = new Annotation(null)
	val vmap = new VariableMap()
    val result = parser(dataStream, annotation, vmap, null, -1, Nil)
     
    expectedResult.zip (result) . 
      foreach { x:(Element,Element) => assertTrue(XMLUtil.compare(x._1,x._2)) }    
  }
  
    // @Test
  def testParseInputWithSequenceOfOne()
  {
    val DFDLSchemaNode = 
      <element name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				<dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" />
			</appinfo>
		</annotation>
        <complexType>
		<sequence>
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" separator="," separatorPosition="postfix"/>
			    </appinfo>
		      </annotation>
			<element name="w" type="int">
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited"/>
			    </appinfo>
		      </annotation>
            </element>
		</sequence>
        </complexType>
	</element>;
    
    val expectedXML = <example1><w>10</w></example1>
 
	val data="10,"
	val dataStream = new RollbackStream(new ByteArrayInputStream(data.getBytes()))
 
	val parser = new SchemaParser().parseElementNode(XMLUtil elem2Element(DFDLSchemaNode), Nil)
     
	val expectedResult = XMLUtil.elem2Element(expectedXML)

    val annotation = new Annotation(null)
	val vmap = new VariableMap()
    val results = parser(dataStream, annotation, vmap, null, -1, Nil)
    
    assertEquals(1, results.length)
    val result = results(0)
    assertTrue(XMLUtil.compare(expectedResult, result))
  }
  
  val wxyzDFDLSchemaNode = 
      <element name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				<dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" />
			</appinfo>
		</annotation>
        <complexType>
		<sequence>
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" separator="," separatorPosition="postfix"/>
			    </appinfo>
		      </annotation>
			<element name="w" type="int">
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" terminator="#"/>
			    </appinfo>
		      </annotation>
            </element>
			<element name="x" type="int">
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" terminator="#"/>
			    </appinfo>
		      </annotation>
            </element>
			<element name="y" type="double">
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" terminator="#"/>
			    </appinfo>
		      </annotation>
            </element>
			<element name="z" type="float">
		      <annotation>
		   	    <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0">
				  <dfdl:format representation="text" encoding="UTF-8" lengthKind="delimited" terminator="#"/>
			    </appinfo>
		      </annotation>
            </element>
		</sequence>
        </complexType>
	</element>;
      
  val wxyzExpectedXML = <example1><w>10</w><x>12</x><y>10.5</y><z>20.5</z></example1>
    
  // @Test
  def testParseInputWithComplexTypeSequence()
  {
 
	val data="10#,12#,10.5#,20.5#,"
	val dataStream = new RollbackStream(new ByteArrayInputStream(data.getBytes()))

 
	val parser = new SchemaParser().parseElementNode(XMLUtil elem2Element(wxyzDFDLSchemaNode), Nil)
     
	val expectedResult = XMLUtil.elem2Element(wxyzExpectedXML)

    val annotation = new Annotation(null)
	val vmap = new VariableMap()
    val results = parser(dataStream, annotation, vmap, null, -1, Nil)
    
    assertEquals(1, results.length)
    val result = results(0)
    assertTrue(XMLUtil.compare(expectedResult, result))
  }
  
  // @Test
   def testVariableDefinition0() {
    val xmlNode = 
      new SAXBuilder().build(new StringReader(
       "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:tns=\"http://www.example.org/example1/\""+
	   "   xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"http://www.example.org/example1/\">"+	
       "	<element name=\"list\" type=\"string\" dfdl:inputValueCalc=\"{ $x }\">"+     
       "		<annotation>"+
       "			<appinfo source=\"http://www.ogf.org/dfdl/dfdl-1.0\">"+
       "				<dfdl:defineVariable name=\"x\" type=\"int\" defaultValue=\"0\"/>"+
       "			</appinfo>"+
       "		</annotation>"+
       "	</element>"+					
       "</schema>")) getRootElement
   
	val parser = new SchemaParser()
	parser parse(xmlNode)
 
	val expectedResult = XMLUtil.element("list","0")

                           
    val result = parser.eval(new ByteArrayInputStream("".getBytes()),"list")
 
    assertTrue(XMLUtil.compare(expectedResult,result))
  }                                  
                         
  // @Test
  def testVariableDefinition() {
    val xmlNode = 
      new SAXBuilder().build(new StringReader(
       "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:tns=\"http://www.example.org/example1/\""+
	   "   xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"http://www.example.org/example1/\">"+	
       "	<element name=\"list\" type=\"example1\" />"+
       "	<complexType name=\"example1\">"+       
       "		<annotation>"+
       "			<appinfo source=\"http://www.ogf.org/dfdl/dfdl-1.0\">"+
       "				<dfdl:defineVariable name=\"x\" type=\"int\" defaultValue=\"0\"/>"+
       "			</appinfo>"+
       "		</annotation>"+
       "		<sequence>"+
       "			<element name=\"w\" type=\"int\" dfdl:inputValueCalc=\"{ $x + 1 }\"/>"+
       "			<element name=\"x\" type=\"int\" dfdl:inputValueCalc=\"{ $x + 2 }\"/>"+
       "			<element name=\"y\" type=\"int\" dfdl:inputValueCalc=\"{ $x + 3 }\"/>"+
       "			<element name=\"z\" type=\"int\" dfdl:inputValueCalc=\"{ $x + 4 }\"/>"+
       "		</sequence>"+
       "	</complexType>"+					
       "</schema>")) getRootElement
   
	val parser = new SchemaParser()
	parser parse(xmlNode)
 
	val expectedResult = XMLUtil.element("list",List(XMLUtil.element("w","1"),XMLUtil.element("x","2"),
                              XMLUtil.element("y","3"),XMLUtil.element("z","4")))

                           
    val result = parser.eval(new ByteArrayInputStream("".getBytes()),"list")
 
    assertTrue(XMLUtil.compare(expectedResult,result))
  }
}
