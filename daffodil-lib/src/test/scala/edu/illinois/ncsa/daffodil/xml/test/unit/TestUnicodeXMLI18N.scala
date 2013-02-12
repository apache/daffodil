package edu.illinois.ncsa.daffodil.xml.test.unit

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */


import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.xml.NS

class TestUnicodeXMLI18N extends JUnitSuite {

  /**
   * Let's characterize the behavior of XML Literals in Scala.
   */
  @Test def testXMLEntities() {
    val fragment = <x>abc&amp;def</x> // XML Entities disappear for us. We never see them. 
    val txt = fragment.text
    assertEquals("abc&def", txt)
  }

  /**
   * Obscure corner about XML. XML isn't allowed to contain character code 0 no way no how, not even as an entity.
   */
  @Test def testNUL1() {
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
  @Test def testUnicodeElementAndAttributeNames() {
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

  def isXS(elem: Node) = NS(elem.namespace) == XMLUtils.XSD_NAMESPACE
  def isSequence(elem: Node) = elem.label == "sequence" && isXS(elem)
  def isSchema(elem: Node) = elem.label == "schema" && isXS(elem)
  def isElement(elem: Node) = elem.label == "element" && isXS(elem)

  val xmlnsURI = XMLUtils.XSD_NAMESPACE
  @Test def testRightElementRightPrefixRightNS() {
    val xsSequence = <xs:sequence xmlns:xs={ xmlnsURI }/>
    assertTrue(isSequence(xsSequence))
  }

  @Test def testRightElementWrongPrefixRightNS() {
    val xsSequence = <wrong:sequence xmlns:wrong={ xmlnsURI }/>
    assertTrue(isSequence(xsSequence))
  }

  @Test def testRightElementRightPrefixWrongNS() {
    val xsSequence = <xs:sequence xmlns:xs="http://Not.the.right.namespace"/>
    assertFalse(isSequence(xsSequence))
  }

  @Test def testRightElementWrongPrefixWrongNS() {
    val xsSequence = <wrong:sequence xmlns:wrong="http://Not.the.right.namespace"/>
    assertFalse(isSequence(xsSequence))
  }

  @Test def testWrongElementRightPrefixRightNS() {
    val xsSequence = <xs:idaho xmlns:xs={ xmlnsURI }/>
    assertFalse(isSequence(xsSequence))
  }

  @Test def testIsSchema() {
    val xsSchema = <xs:schema xmlns:xs={ xmlnsURI }/>
    assertTrue(isSchema(xsSchema))
  }

  @Test def testIsElement() {
    val xsElement = <xs:element xmlns:xs={ xmlnsURI }/>
    assertTrue(isElement(xsElement))
  }

}