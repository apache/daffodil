package daffodil.schema.test.integration

import java.io.ByteArrayInputStream
import java.io.StringReader

import org.jdom.Element
import org.jdom.Text
import org.jdom.input.SAXBuilder
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.parser.SchemaParser
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.Hidden
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil.compare


class HiddenElementTest extends FunSuite with ShouldMatchers {

	test("basic hidden element test 1")	{
	  val builder = new SAXBuilder()
	  val schema = builder.build(new StringReader( 
			"<complexType name=\"mytype\" xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\">"+
			"	<sequence>"+
			"		<annotation>"+
			"			<appinfo source=\"http://www.ogf.org/dfdl/\">"+
			"				<dfdl:hidden>"+
			"					<element name=\"nested1\" type=\"int\"/>"+
			"				</dfdl:hidden>"+
			"			</appinfo>"+
			"		</annotation>"+
			"		<element name=\"nested2\" type=\"int\"/>"+
			"		<element name=\"nested3\" type=\"string\"/>"+
			"	</sequence>"+
			"</complexType>")) getRootElement


		val a1 = new Annotation(null)
		a1.format setType("int")
		a1.hidden = new Hidden()
		
		val a2 = new Annotation(null)
		a2.format setType("int")
		
		val a3 = new Annotation(null)
		a3.format setType("string")

		val expectedResult = new Sequence(new Annotation(null),null,new Namespaces,List(
				new SimpleElement("nested1",a1,null,new Namespaces),
				new SimpleElement("nested2",a2,null,new Namespaces),
				new SimpleElement("nested3",a3,null,new Namespaces)))


		val t = new SchemaParser() parseComplexType(schema)
		t should equal (expectedResult)
	}
  
  test("full test 1"){
    val builder = new SAXBuilder()
    val schema = builder.build(new StringReader( 
    	"<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"myNamespace\">"+
    	"	<complexType name=\"mytype\">"+
    	"		<annotation>"+
    	"			<appinfo source=\"http://www.ogf.org/dfdl/\">"+
    	"				<dfdl:format representation=\"text\" separator=\",\" terminator=\"&#x0A; &#x0D;&#x0A;\"/>"+
    	"			</appinfo>"+
    	"		</annotation>"+
    	"		<sequence>"+
    	"			<annotation>"+
    	"				<appinfo source=\"http://www.ogf.org/dfdl/\">"+
    	"					<dfdl:hidden>"+
    	"						<element name=\"nested1\" type=\"int\"/>"+
    	"					</dfdl:hidden>"+
    	"				</appinfo>"+
    	"			</annotation>"+
    	"			<element name=\"nested2\" type=\"int\"/>"+
    	"			<element name=\"nested3\" type=\"string\"/>"+
    	"		</sequence>"+
    	"	</complexType>"+
    	"	<element name=\"myElement\" type=\"mytype\"/>"+
        "</schema>")) getRootElement
      
      val text = "50,100,some text"
      
      
      val expectedResult = new Element("myElement")
      val e1 = new Element("nested2")
      val e2 = new Element("nested3")
      e1 addContent(new Text("100"))
      e2 addContent(new Text("some text"))
      expectedResult addContent(e1)
      expectedResult addContent(e2)
            
      val parser = new SchemaParser()
      parser parse(schema)      
      
      val actualResult = parser eval(new ByteArrayInputStream(text getBytes),"myElement")
                  
      compare(expectedResult,actualResult) should be (true)
  }
  
  test("full test 2"){
    val builder = new SAXBuilder()
    val schema = builder.build(new StringReader(  
    	"<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"myNamespace\">"+
    	"	<complexType name=\"mytype\">"+
    	"		<annotation>"+
    	"			<appinfo source=\"http://www.ogf.org/dfdl/\">"+
    	"				<dfdl:format representation=\"text\" separator=\",\" terminator=\"&#x0A; &#x0D;&#x0A;\"/>"+
    	"			</appinfo>"+
    	"		</annotation>"+
    	"		<sequence>"+
    	"			<annotation>"+
    	"				<appinfo source=\"http://www.ogf.org/dfdl/\">"+
    	"					<dfdl:hidden>"+ 
    	"						<element name=\"nested1\" type=\"int\"/>"+
    	"					</dfdl:hidden>"+
    	"				</appinfo>"+
    	"			</annotation>"+
    	"			<element name=\"nested2\" type=\"int\"/>"+
    	"			<element name=\"nested3\" type=\"string\"/>"+
    	""+
    	"  			<element name=\"times2\" type=\"int\" dfdl:inputValueCalc=\"{ ../nested1 * 2 }\"/>"+
    	" 			<element name=\"times3\" type=\"int\" dfdl:inputValueCalc=\"{ ../nested1 * 3 }\"/>"+
    	"" +
    	"   	</sequence>"+
    	"  	</complexType>"+
    	"	<element name=\"myElement\" type=\"mytype\"/>"+
    	"</schema>")) getRootElement
      
      val text = "50,100,some text"
      
      
      val expectedResult = new Element("myElement")
      
      val e1 = new Element("nested2")
      val e2 = new Element("nested3")
      val e3 = new Element("times2")
      val e4 = new Element("times3")
      e1 addContent(new Text("100"))
      e2 addContent(new Text("some text"))
      e3 addContent(new Text("100"))
      e4 addContent(new Text("150"))
      
      expectedResult addContent(e1)
      expectedResult addContent(e2)
      expectedResult addContent(e3)
      expectedResult addContent(e4)
            
      val parser = new SchemaParser()
      parser parse(schema)      
      
      val actualResult = parser eval(new ByteArrayInputStream(text getBytes),"myElement")
               
      compare(expectedResult,actualResult) should be (true)
  }
  
}
