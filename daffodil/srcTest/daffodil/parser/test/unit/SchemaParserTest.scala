package daffodil.parser.test.unit

import java.io.ByteArrayInputStream
import java.io.StringReader
import scala.xml._

import org.jdom.Element
import org.jdom.input.SAXBuilder
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.processors.VariableMap
import daffodil.schema.Sequence
import daffodil.schema.SimpleElement
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.enumerations.Text
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil

class SchemaParserTest extends FunSuite with ShouldMatchers {

  test("parse simple element") (pending)
  
  test("parse simple extended element") (pending)
  
  test("parse complexType/choice") (pending)
  
  test("parse complexType/sequence") {
    val xmlNode = 
      <complexType name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/">
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
    
    val a1 = new Annotation(null); a1.format setType("int")
    val a2 = new Annotation(null); a2.format setType("int")
    val a3 = new Annotation(null); a3.format setType("double")
    val a4 = new Annotation(null); a4.format setType("float")
    
    val expectedResult = new Sequence(a0,"example.com:example",new Namespaces,
    List(new SimpleElement("w",a1,"example.com:example",new Namespaces),
         new SimpleElement("x",a2,"example.com:example",new Namespaces),
         new SimpleElement("y",a3,"example.com:example",new Namespaces),
         new SimpleElement("z",a4,"example.com:example",new Namespaces)))
    
    result should equal (expectedResult)
  }
  
  test("parse complexType/sequence II") {
    val xmlNode = 
      <complexType name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/">
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
    
    parser.isInstanceOf[Sequence] should be (true)
    parser.annotation should equal (a0)
        
    val children = parser.asInstanceOf[Sequence].getChildren
    children(0).isInstanceOf[SimpleElement] should be (true)
    children(1).isInstanceOf[SimpleElement] should be (true)
    children(2).isInstanceOf[SimpleElement] should be (true)
    children(3).isInstanceOf[SimpleElement] should be (true)
    
    children(0).asInstanceOf[SimpleElement].name should equal ("w")
    children(1).asInstanceOf[SimpleElement].name should equal ("x")
    children(2).asInstanceOf[SimpleElement].name should equal ("y")
    children(3).asInstanceOf[SimpleElement].name should equal ("z")
    
    val a1 = new Annotation(null); a1.format setType("int")
    val a2 = new Annotation(null); a2.format setType("int")
    val a3 = new Annotation(null); a3.format setType("double")
    val a4 = new Annotation(null); a4.format setType("float")
    
    children(0).annotation should equal (a1)
    children(1).annotation should equal (a2)
    children(2).annotation should equal (a3)
    children(3).annotation should equal (a4)    
  }
  
  
  
  test("parse input with complexType/sequence") 
  {
    val xmlNode = 
      <complexType name="example1" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
		<annotation>
			<appinfo source="http://www.ogf.org/dfdl/">
				<dfdl:format representation="text" encoding="UTF-8" separator="," numberDecimalSeparator="##"/>
			</appinfo>
		</annotation>
		<sequence>
			<element name="w" type="int"/>
			<element name="x" type="int"/>
			<element name="y" type="double"/>
			<element name="z" type="float"/>
		</sequence>
	</complexType>;
 
	val data="10,12,10.5,20.5"
 
	val parser = new SchemaParser().parseComplexType(XMLUtil elem2Element(xmlNode))
     
	val expectedResult = List(XMLUtil.element("w","10"),XMLUtil.element("x","12"),
                              XMLUtil.element("y","10.5"),XMLUtil.element("z","20.5"))

    val result = parser(new RollbackStream(new ByteArrayInputStream(data.getBytes())),
                     	new Annotation(null),new VariableMap(),null,-1,Nil)
     
    expectedResult.zip (result) . 
      forall { x:(Element,Element) => XMLUtil.compare(x._1,x._2)} should be (true)	    
  }
  
  test("parse input with schema/complexType/sequence") {
    val xmlNode = 
      <schema xmlns="http://www.w3.org/2001/XMLSchema" xmlns:tns="http://www.example.org/example1/"
	      xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0" targetNamespace="http://www.example.org/example1/">	
			<element name="list" type="example1" />			
         	<complexType name="example1">
				<annotation>
					<appinfo source="http://www.ogf.org/dfdl/">
						<dfdl:format representation="text" encoding="UTF-8"
							separator="," />
					</appinfo>
				</annotation>
				<sequence>
					<element name="w" type="int"></element>
					<element name="x" type="int"></element>
					<element name="y" type="double"></element>
					<element name="z" type="float"></element>
				</sequence>
			</complexType>		
			
	  </schema>;
   
	val data="10,12,10.5,20.5"
 
	val parser = new SchemaParser()
	parser parse(XMLUtil elem2Element(xmlNode))
     
	val expectedResult = XMLUtil.element("list",List(XMLUtil.element("w","10"),XMLUtil.element("x","12"),
                              XMLUtil.element("y","10.5"),XMLUtil.element("z","20.5")))
 
	val result = parser.eval(new ByteArrayInputStream(data.getBytes()),"list")
 
	XMLUtil.compare(expectedResult,result) should be (true)

  }
  
   test("variable definition 0") {
    val xmlNode = 
      new SAXBuilder().build(new StringReader(
       "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:tns=\"http://www.example.org/example1/\""+
	   "   xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"http://www.example.org/example1/\">"+	
       "	<element name=\"list\" type=\"string\" dfdl:inputValueCalc=\"{ $x }\">"+     
       "		<annotation>"+
       "			<appinfo source=\"http://www.ogf.org/dfdl/\">"+
       "				<dfdl:defineVariable name=\"x\" type=\"int\" defaultValue=\"0\"/>"+
       "			</appinfo>"+
       "		</annotation>"+
       "	</element>"+					
       "</schema>")) getRootElement
   
	val parser = new SchemaParser()
	parser parse(xmlNode)
 
	val expectedResult = XMLUtil.element("list","0")

                           
    val result = parser.eval(new ByteArrayInputStream("".getBytes()),"list")
 
    XMLUtil.compare(expectedResult,result) should be (true)
  }                                  
                                                     
  test("variable definition") {
    val xmlNode = 
      new SAXBuilder().build(new StringReader(
       "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:tns=\"http://www.example.org/example1/\""+
	   "   xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"http://www.example.org/example1/\">"+	
       "	<element name=\"list\" type=\"example1\" />"+
       "	<complexType name=\"example1\">"+       
       "		<annotation>"+
       "			<appinfo source=\"http://www.ogf.org/dfdl/\">"+
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
 
    XMLUtil.compare(expectedResult,result) should be (true)
  }
}
