package daffodil.xml.test.unit

import java.io.StringReader
import scala.xml._

import org.jdom.input.SAXBuilder
import daffodil.xml.XMLUtil
import org.scalatest.junit.JUnit3Suite
import scala.collection.mutable.ListBuffer
import junit.framework.Assert._

class XMLUtilTest extends JUnit3Suite {

  // @Test
  def testElem2Element() {
    
    val schema = 
    	<complexType name="mytype" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0">
			<sequence>
				<annotation>
					<appinfo>
						<dfdl:hidden>
							<element name="nested1" type="int"/>
						</dfdl:hidden>
					</appinfo>
				</annotation>
				<element name="nested2" type="int"/>
				<element name="nested3" type="string"/>
			</sequence>
		</complexType>
  
	println("******************  ONE")
	println(schema)
 
	println("******************  TWO")
 
	val other = XMLUtil.elem2Element(schema)
	// println(XMLUtil toString(other))
	println(other.toString)
  }
  
  // @Test
  def testGetElementByName() {
    val xmlNode = 
      new SAXBuilder().build(new StringReader(
      "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:tns=\"http://www.example.org/example1/\""+
	  "    xmlns:dfdl=\"http://www.ogf.org/dfdl/dfdl-1.0\" targetNamespace=\"http://www.example.org/example1/\">"+	
      "		<complexType name=\"example1\">"+
      "				<annotation>"+
	  "					<appinfo>"+
	  "						<dfdl:format representation=\"text\" encoding=\"UTF-8\""+
	  "							separator=\",\" />"+
	  "					</appinfo>"+
	  "				</annotation>"+
	  "			<sequence>"+
	  "				<element name=\"w\" type=\"int\"></element>"+
	  "				<element name=\"x\" type=\"int\"></element>"+
	  "				<element name=\"y\" type=\"double\"></element>"+
	  "				<element name=\"z\" type=\"float\"></element>"+
	  "			</sequence>"+
	  "		</complexType>"+		
	  "		<element name=\"list\" type=\"example1\"/>"+
	  "</schema>")) getRootElement
   
	  assertTrue(XMLUtil.getChildByName(xmlNode,"example1") != null)
   
  }
}
