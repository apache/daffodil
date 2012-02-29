package daffodil.xml.test.unit

import java.io.StringReader
import scala.xml._

import org.jdom.input.SAXBuilder
import daffodil.xml.XMLUtil
import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class XMLUtilTest extends JUnit3Suite {

  val xsd = XMLUtil.XSD_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val ex = XMLUtil.EXAMPLE_NAMESPACE

  // @Test
  def testElem2Element1() {

    val schema = <element name="nested1" type="int" xmlns={ xsd }/>

    println("******************  ONE")
    println(schema)

    println("******************  TWO")

    val other = XMLUtil.elem2Element(schema)
    val otherString = XMLUtil.serializeCompact(other)
    val actual = XML.loadString(otherString)
    println(actual)
    assertEquals(schema, actual)
  }

  def testScalaXMLEquality() {
    val elt1 = <foobar attr1="foo" attr2="bar"/>
    val elt2 = <foobar attr2="bar" attr1="foo"/> // attribute order is not significant in XML according to w3c
    assertEquals(elt1, elt2)
  }

  def testCompactXml1() {
    val xml: NodeSeq = <foobar><foo>notWhitespace</foo><foo></foo></foobar>
    val expected: NodeSeq = <foobar><foo>notWhitespace</foo><foo></foo></foobar>
    assertEquals(expected, xml)
    val actual = XMLUtil.compactXml(xml)
    val expected2ndChild = (expected \ "foo")(1)
    val actual2ndChild = (actual \ "foo")(1)
    assertEquals(expected2ndChild, actual2ndChild)
    val expected1stChild = (expected \ "foo")(0)
    val actual1stChild = (actual \ "foo")(0)
    assertEquals(expected1stChild, actual1stChild)
    val expectedAsString = expected.toString
    val actualAsString = actual.toString
    assertEquals(expectedAsString, actualAsString) // they are string equal, but not equals equal...???
    val pairs = expected zip actual
    pairs.map {
      case (exp, act) => {
        exp match {
          case scala.xml.Elem(expPrefix, expLabel, expAttributes, expScope, expChildren @ _*) => {
            act match {
              case scala.xml.Elem(actPrefix, actLabel, actAttributes, actScope, actChildren @ _*) => {
                assertEquals(expPrefix, actPrefix)
                assertEquals(expLabel, actLabel)
                assertEquals(expAttributes, actAttributes)
                assertEquals(expScope, actScope)
                assertEquals(expChildren, actChildren)
              }
              case _ => fail()
            }
          }
          case _ => assertEquals(exp, act)
        }
      }
    }
    //gaak. The top level structures are of different types. Coerce both to list.
    assertEquals(expected.toList, actual.toList)
  }

  def testElem2Element2() {

    val schema = XMLUtil.compactXml(
      <dfdl:hidden xmlns:dfdl={ dfdl }>
        <element name="nested1" type="int" xmlns={ xsd }/>
      </dfdl:hidden>)

    println("******************  ONE")
    println(schema)

    println("******************  TWO")

    val other = XMLUtil.elem2Element(schema(0))
    val otherString = XMLUtil.serializeCompact(other)
    val actual: scala.xml.NodeSeq = XML.loadString(otherString).toList
    println(actual)
    val expected = schema.toList
    assertEquals(schema, actual)
  }

  // @Test
  def testElem2ElementComplexType() {
    val expected = XMLUtil.compactXml(
      <complexType name="mytype" xmlns={ xsd }>
        <sequence>
          <annotation>
            <appinfo>
              <dfdl:hidden xmlns:dfdl={ dfdl }>
                <element type="int" name="nested1"/>
              </dfdl:hidden>
            </appinfo>
          </annotation>
          <element type="int" name="nested2"/>
          <element type="string" name="nested3"/>
        </sequence>
      </complexType>)

    println("******************  ONE")
    println(expected)

    println("******************  TWO")

    val other = XMLUtil.elem2Element(expected.head)
    val otherString = XMLUtil.serializeCompact(other)
    val actual = XML.loadString(otherString)
    assertEquals(expected.toList, actual.toList)
    // println(XMLUtil toString(other))
    println(other.toString)
  }

  def testElem2ElementComplexElement() {

    val expected = XMLUtil.compactXml(
      <element name="myElement">
        <complexType name="mytype" xmlns={ xsd } xmlns:dfdl={ dfdl }>
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
      </element>)

    println("******************  ONE")
    println(expected)

    println("******************  TWO")

    val other = XMLUtil.elem2Element(expected)
    // println(XMLUtil toString(other))
    println(other.toString)
    val otherString = XMLUtil.serializeCompact(other(0))
    val actual = XML.loadString(otherString)
    assertEquals(expected.toList, actual.toList)

  }

  // @Test
  def testGetElementByName() {
    val xmlNode =
      <schema xmlns={ xsd } targetNamespace={ ex } xmlns:tns={ ex } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <complexType name="example1">
          <annotation>
            <appinfo>
              <dfdl:format representation="text" encoding="UTF-8" separator=","/>
            </appinfo>
          </annotation>
          <sequence>
            <element name="w" type="int"></element>
            <element name="x" type="int"></element>
            <element name="y" type="double"></element>
            <element name="z" type="float"></element>
          </sequence>
        </complexType>
        <element name="list" type="example1"/>
      </schema>

    val jdomNode = XMLUtil.elem2Element(xmlNode)

    assertTrue(XMLUtil.getChildByName(jdomNode, "example1") != null)

  }

  // @Test
  def testRemoveAttributes1() {
    val testData = <foo bar="baz"/>
    val actual = XMLUtil.removeAttributes(testData)
    assertEquals(<foo/>, actual)
  }

  // @Test
  def testRemoveAttributes2() {
    val testData = <foo bar="baz"><quux foo="bar"/></foo>
    val actual = XMLUtil.removeAttributes(testData)
    assertEquals(<foo><quux/></foo>, actual)
  }

}
