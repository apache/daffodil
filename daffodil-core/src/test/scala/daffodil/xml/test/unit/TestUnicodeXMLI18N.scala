package daffodil.xml.test.unit

import scala.xml._

import daffodil.xml.XMLUtils
import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class TestUnicodeXMLI18N extends JUnit3Suite {
  
  /**
   * Let's characterize the behavior of XML Literals in Scala.
   */
 def testXMLEntities() {
    val fragment = <x>abc&amp;def</x> // XML Entities disappear for us. We never see them. 
    val txt = fragment.text
    assertEquals("abc&def", txt)
  }

 /**
  * Obscure corner about XML. XML isn't allowed to contain character code 0 no way no how, not even as an entity.
  */
  def testNUL1() {
    val fragment = <x>abc&#x0000;def</x> // This isn't supposed to work in XML. character code 0 is never allowed in XML documents.
    // TODO: implement our own enforcement of disallowing NUL in XML infoset strings.
    // TODO: consider XML v1.0 versus v1.1 - infosets allow different character codes. 1.1 only disallows 0.
    val txt = fragment.text
    assertEquals("abc\000def", txt)
    assertEquals(7, txt.length)
  }

  /**
   * if this test doesn't pass, your environment is not adequately set up for Unicode and internationalized
   * characters
   */
  def testUnicodeElementAndAttributeNames() {
//    val fragment = <Date 年月日="2003年08月27日">2003年08月27日<SUB2003年08月27日>hi</SUB2003年08月27日></Date> // <!-- element contains unicode in its name -->
//    val txt = fragment.text
//    assertEquals("2003年08月27日hi", txt)
//    assertEquals(13, txt.length)
//    val subfrag = (fragment \ "SUB2003年08月27日").text
//    assertEquals("hi", subfrag)
//    val attr = (fragment \ "@年月日").text
//    assertEquals("2003年08月27日", attr)
  }

  /**
   * These next tests characterize Scala's namespace handling in XML literals
   * If these tests break, then that means they have changed the behavior of XML literals in Scala
   * in a way that will break daffodil (or is likely to break it.)
   */ 
  
  def isXS(elem : Node) = elem.namespace == XMLUtils.XSD_NAMESPACE
  def isSequence(elem : Node) = elem.label == "sequence" && isXS(elem)
  def isSchema(elem : Node) = elem.label == "schema" && isXS(elem)
  def isElement(elem : Node) = elem.label == "element"  && isXS(elem)

  val xmlnsURI = XMLUtils.XSD_NAMESPACE
  def testRightElementRightPrefixRightNS() {
    val xsSequence = <xs:sequence xmlns:xs={xmlnsURI}/>
    assertTrue(isSequence(xsSequence))
  }

  def testRightElementWrongPrefixRightNS() {
    val xsSequence = <wrong:sequence xmlns:wrong={xmlnsURI}/>
    assertTrue(isSequence(xsSequence))
  }

  def testRightElementRightPrefixWrongNS() {
    val xsSequence = <xs:sequence xmlns:xs="http://Not.the.right.namespace"/>
    assertFalse(isSequence(xsSequence))
  }

  def testRightElementWrongPrefixWrongNS() {
    val xsSequence = <wrong:sequence xmlns:wrong="http://Not.the.right.namespace"/>
    assertFalse(isSequence(xsSequence))
  }

  def testWrongElementRightPrefixRightNS() {
    val xsSequence = <xs:idaho xmlns:xs={xmlnsURI}/>
    assertFalse(isSequence(xsSequence))
  }

  def testIsSchema() {
    val xsSchema = <xs:schema xmlns:xs={xmlnsURI}/>
    assertTrue(isSchema(xsSchema))
  }

  def testIsElement() {
    val xsElement = <xs:element xmlns:xs={xmlnsURI}/>
    assertTrue(isElement(xsElement))
  }

}