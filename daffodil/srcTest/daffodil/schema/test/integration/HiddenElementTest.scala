package daffodil.schema.test.integration

import java.io.ByteArrayInputStream
import java.io.BufferedInputStream
import java.io.StringReader

import org.jdom.Element
import org.jdom.Text
import org.jdom.input.SAXBuilder
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.parser.SchemaParser
import daffodil.schema._
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.Hidden
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil
import daffodil.xml.XMLUtil.compare
import daffodil.Implicits._

import org.scalatest.junit.JUnit3Suite
import scala.collection.mutable.ListBuffer
import junit.framework.Assert._

class HiddenElementTest extends JUnit3Suite with ShouldMatchers {
  
  def testBasicHiddenElement1 () { 
	 
	  val xml = XMLUtil.compactXml(
			<complexType name="mytype" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
				<sequence>
					<annotation>
						<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
							<dfdl:hidden>
								<element name="nested1" type="int"/>
							</dfdl:hidden>
						</appinfo>
					</annotation>
				</sequence>
			</complexType>)
	    val schema = XMLUtil.elem2Element(xml)(0)

		val a1 = new Annotation(null)
		a1.format setTypeName("int")
		a1.hidden = new Hidden()

		val expectedResult = new Sequence(new Annotation(null),null,new Namespaces,List(
				new SimpleElement("nested1",a1,null,new Namespaces)))

		val t = new SchemaParser() parseComplexType(schema)
		
		t should equal (expectedResult)
	}

	def testBasicHiddenElement2 () { // ("basic hidden element test 1")	{
	 
	  val xml = XMLUtil.compactXml(
			<complexType name="mytype" xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
				<sequence>
					<annotation>
						<appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
							<dfdl:hidden>
								<element name="nested1" type="int"/>
							</dfdl:hidden>
						</appinfo>
					</annotation>
					<element name="nested2" type="int"/>
					<element name="nested3" type="string"/>
				</sequence>
			</complexType>)
	    val schema = XMLUtil.elem2Element(xml)(0)


		val a1 = new Annotation(null)
		a1.format setTypeName("int")
		a1.hidden = new Hidden()
		
		val a2 = new Annotation(null)
		a2.format setTypeName("int")
		
		val a3 = new Annotation(null)
		a3.format setTypeName("string")

		val expectedResult = new Sequence(new Annotation(null),null,new Namespaces,List(
				new SimpleElement("nested1",a1,null,new Namespaces),
				new SimpleElement("nested2",a2,null,new Namespaces),
				new SimpleElement("nested3",a3,null,new Namespaces)))


		val t = new SchemaParser() parseComplexType(schema)
		
		t should equal (expectedResult)
	}
  
  def testFull1 () { // ("full test 1"){
    val xml =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0" targetNamespace="myNamespace">
        <element name="myElement">
          <annotation>
            <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
              <dfdl:format representation="text" terminator="$"/>
            </appinfo>
          </annotation>
          <complexType>
          <sequence>
            <annotation>
              <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                <dfdl:format representation="text" separator="," separatorPosition="infix"/>
                <dfdl:hidden>
                  <element name="nested1" type="int">
                    <annotation>
                      <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                        <dfdl:format representation="text" terminator="!"/>
                      </appinfo>
                    </annotation> 
                   </element>
                </dfdl:hidden>
              </appinfo>
            </annotation>
              <!-- <element name="nested1" type="int">
                    <annotation>
                      <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                        <dfdl:format representation="text" terminator="!"/>
                      </appinfo>
                    </annotation>
                  </element> -->
            <element name="nested2" type="int">
              <annotation>
                <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                  <dfdl:format representation="text" terminator="!"/>
                </appinfo>
              </annotation>
            </element>
            <element name="nested3" type="string">
              <annotation>
                <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                  <dfdl:format representation="text" terminator="!"/>
                </appinfo>
              </annotation>
            </element>
          </sequence>
          </complexType>
        </element>
        <!-- <element name="myElement" type="mytype"/> -->
      </schema>
      val schema = XMLUtil.elem2Element(xml)
      
      val text = "50!,100!,some text!$"
            
      val parser = new SchemaParser()
      parser parse(schema)      
      val datastream = new ByteArrayInputStream(text getBytes)
      val actualResult = parser eval(datastream,"myElement")
    
      val res = XMLUtil.element2Elem(actualResult)
      
      val n1 = res \ "nested1" text
      val n2 = res \ "nested2" text
      val n3 = res \ "nested3" text ;
      assertEquals("50", n1)
      assertEquals("100", n2)
      assertEquals("some text", n3)
  }
  
  def testFull2 () { // ("full test 2"){
    val xml =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" targetNamespace="myNamespace">
        <complexType name="mytype">
          <sequence>
            <annotation>
              <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                <dfdl:format representation="text" separator="," separatorPosition="infix"/>
                <dfdl:hidden>
                  <element name="nested1" type="int">
                    <annotation>
                      <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                        <dfdl:format representation="text" terminator="!"/>
                      </appinfo>
                    </annotation>
                  </element>
                </dfdl:hidden>
              </appinfo>
            </annotation>
            <element name="nested2" type="int">
              <annotation>
                <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                  <dfdl:format representation="text" terminator="!"/>
                </appinfo>
              </annotation>
            </element>
            <element name="nested3" type="string">
              <annotation>
                <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                  <dfdl:format representation="text" terminator="!"/>
                </appinfo>
              </annotation>
            </element>
            <element name="times2" type="int" dfdl:inputValueCalc="{ ../nested1 * 2 }"/>
            <element name="times3" type="int" dfdl:inputValueCalc="{ ../nested1 * 3 }"/>
          </sequence>
        </complexType>
        <element name="myElement" type="mytype">
        </element>
      </schema>
      val schema = XMLUtil.elem2Element(xml)
      
      val text = "50!,100!,some text!$"
            
      val parser = new SchemaParser()
      parser parse(schema)      
      
      val datastream = new ByteArrayInputStream(text getBytes)
      val actualResult = parser eval(datastream,"myElement")
      val res = XMLUtil.element2Elem(actualResult)
      
      val n2 = res \ "nested2" text
      val n3 = res \ "nested3" text
      val t2 = res \ "times2" text
      val t3 = res \ "times3" text
      ; // this semicolon keeps eclipse happy
      assertEquals("100", n2)
      assertEquals("some text", n3)
      assertEquals("100", t2)
      assertEquals("150", t3)
  }
  
}
