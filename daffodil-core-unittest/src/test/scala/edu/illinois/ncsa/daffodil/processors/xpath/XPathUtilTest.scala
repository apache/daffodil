package edu.illinois.ncsa.daffodil.processors.xpath

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

import org.jdom.Document
import org.jdom.Element
import org.jdom.Text
import scala.math.Pi
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.xml._
import junit.framework.Assert._
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathExpressionException
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class XPathUtilTest {

  @Test def testXPath1() {

    // <root><child1><child2><child3>19</child3></child2></child1></root>

    val text = new Text("19")
    val child3 = new Element("child3")
    val child2 = new Element("child2")
    val child1 = new Element("child1")
    val root = new Element("root")

    child3 addContent (text)
    child2 addContent (child3)
    child1 addContent (child2)
    root addContent (child1)

    val document = new Document(root)

    val result = XPathUtil evalExpressionFromString ("/root/child1/child2/child3", new VariableMap(), root, Nil)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => fail
    }
  }

  @Test def testXPath2() {

    /*    	
     * <root>
     * 		<level1_1>
     * 			<level2_1>
     * 				<level3_1>19</level3_1>
     * 			</level2_1>
     * 		</level1_1>
     * 		<level1_2>
     * 			<level2_2>
     * 				<level3_2>42</level3_2>
     * 			</level2_2>
     *  	<level1_2>
     * </root>
     
     */

    val level3_1 = new Element("level3_1")
    val level3_2 = new Element("level3_2")
    val level2_1 = new Element("level2_1")
    val level2_2 = new Element("level2_2")
    val level1_1 = new Element("level1_1")
    val level1_2 = new Element("level1_2")
    val root = new Element("root")

    root addContent (level1_1)
    root addContent (level1_2)
    level1_1 addContent (level2_1)
    level1_2 addContent (level2_2)
    level2_1 addContent (level3_1)
    level2_2 addContent (level3_2)
    level3_1 addContent (new Text("19"))
    level3_2 addContent (new Text("42"))

    val document = new Document(root)
    val result = XPathUtil evalExpressionFromString ("../../level1_2/level2_2/level3_2", new VariableMap(), level2_1, Nil)

    result match {
      case NodeResult(x) => assertEquals("42", x.getText)
      case _ => fail
    }
  }

  /**
   * Illustrates xpath with QNames so it is specific about the
   * namespaces.
   */
  @Test def testXPathWithQNamesInXPathForNamespaces() {

    // <f:root xmlns:f="foobarNS><f:child1>19</f:child1></f:root>
    val text = new Text("19")
    val child1 = new Element("child1", "urn:foobarNS")
    val root = new Element("root", "urn:foobarNS")
    child1 addContent (text)
    root addContent (child1)
    val document = new Document(root)

    val ns = List(org.jdom.Namespace.getNamespace("f", "urn:foobarNS"))
    val result = XPathUtil evalExpressionFromString ("/f:root/f:child1",
      new VariableMap(),
      root,
      ns)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        // println(result)
        fail
      }
    }
  }

  /**
   * This illustrates the other way of getting the namespace information.
   * If the JDOM Elements are constructed with the 3-arg constructor that takes
   * both a prefix and a namespace uri (and the name), then one can get the namespaces
   * directly from the element itself.
   */
  @Test def testXPathWithQNamesInXPathForNamespaces2() {

    // <f:root xmlns:f="foobarNS><f:child1>19</f:child1></f:root>
    val text = new Text("19")
    val child1 = new Element("child1", "f", "urn:foobarNS")
    val root = new Element("root", "f", "urn:foobarNS")
    child1 addContent (text)
    root addContent (child1)
    val document = new Document(root)

    val ns = XMLUtils.namespaceBindings(root)
    val result = XPathUtil evalExpressionFromString ("/f:root/f:child1",
      new VariableMap(),
      root,
      ns)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        // println(result)
        fail
      }
    }
  }

  /**
   * Test if converting the XML from Scala's representation changes anything.
   */
  @Test def testXPathWithQNamesInXPathForNamespaces3() {

    val root = XMLUtils.elem2Element(
      <f:root xmlns:f="urn:foobarNS"><f:child1>19</f:child1></f:root>)
    val document = new Document(root)

    val ns = XMLUtils.namespaceBindings(root)
    val result = XPathUtil evalExpressionFromString ("/f:root/f:child1",
      new VariableMap(),
      root,
      ns)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        // println(result)
        fail
      }
    }
  }

  /**
   * Test text() in paths
   */
  @Test def testXPathWithTextInPath() {

    val root = XMLUtils.elem2Element(
      <f:root xmlns:f="urn:foobarNS"><f:child1>19</f:child1></f:root>)
    //    val text = new Text("19")
    //    val child1 = new Element("child1", "f", "urn:foobarNS")
    //    val root = new Element("root", "f", "urn:foobarNS")
    //    child1 addContent(text)
    //    root addContent(child1)
    val document = new Document(root)

    val ns = XMLUtils.namespaceBindings(root)
    val result: XPathResult = XPathUtil evalExpressionFromString ("/f:root/f:child1/text()",
      new VariableMap(),
      root,
      ns,
      XPathConstants.STRING)

    result match {
      case StringResult(x) => assertEquals("19", x)
      case _ => {
        // println(result)
        fail
      }
    }
  }

  /**
   * We always get back "" on failures, even if the path is invalid and the node cannot exist.
   */
  @Test def testXPathForString() {

    val text = new Text("ABC")
    val root = new Element("root")
    root addContent (text)

    val document = new Document(root)
    val vars = new VariableMap()
    var result: XPathResult = XPathUtil evalExpressionFromString ("/root/text()", vars, root, Nil, XPathConstants.STRING)
    assertEquals(StringResult("ABC"), result)
    result = XPathUtil evalExpressionFromString ("/root", vars, root, Nil, XPathConstants.STRING)
    assertEquals(StringResult("ABC"), result)
  }

  @Test def testXPathForEmptyString() {
    val root = new Element("root")
    val document = new Document(root)

    val vars = new VariableMap()
    var result: XPathResult = XPathUtil evalExpressionFromString ("/root", vars, root, Nil, XPathConstants.STRING)
    assertEquals(StringResult(""), result)

    val e = intercept[XPathExpressionException] {
      result = XPathUtil evalExpressionFromString ("/foobar", vars, root, Nil, XPathConstants.STRING)
    }
    assertTrue(e.getMessage().contains("foobar"))

  }

  /**
   * We always get back a NaN on failures. Even if the path itself is invalid and the value can't exist.
   */
  @Test def testXPathForNumber() {

    val text = new Text("19")
    val root = new Element("root")
    root addContent (text)

    val document = new Document(root)
    val vars = new VariableMap()
    var result: XPathResult = XPathUtil evalExpressionFromString ("/root/text()", vars, root, Nil, XPathConstants.NUMBER)
    assertEquals(NumberResult("19"), result)
    result = XPathUtil evalExpressionFromString ("/root", vars, root, Nil, XPathConstants.NUMBER)
    assertEquals(NumberResult("19"), result)
  }

  @Test def testXPathForNumberFailures() {

    val text = new Text("ABC")
    val root = new Element("root")
    root addContent (text)

    val document = new Document(root)
    val vars = new VariableMap()
    var result: XPathResult = XPathUtil evalExpressionFromString ("/root", vars, root, Nil, XPathConstants.NUMBER)
    result match {
      case NotANumberResult(n) => // OK
      case NumberResult(n) => fail
      case _ => fail
    }
    result = XPathUtil evalExpressionFromString ("/root", vars, root, Nil, XPathConstants.NODE)
    result match {
      case NodeResult(n) => {
        assertEquals("root", n.getName())
      }
      case _ => fail
    }
    val e = intercept[XPathExpressionException] {
      result = XPathUtil evalExpressionFromString ("/foobar", vars, root, Nil, XPathConstants.NUMBER)
      result
    }
    assertTrue(e.getMessage().contains("foobar"))
  }

  /**
   *
   */
  @Test def testXPathForConstantPaths() {

    //    val root = new Element("root")
    //    val document = new Document(root)

    val vars = new VariableMap()
    var result: XPathResult = XPathUtil evalExpressionFromString (" 19 ", vars, null, Nil, XPathConstants.NUMBER)
    assertEquals(NumberResult("19"), result)
    result = XPathUtil evalExpressionFromString (" 19 * 47 ", vars, null, Nil, XPathConstants.NUMBER)
    assertEquals(NumberResult((19 * 47).toString), result)
    result = XPathUtil evalExpressionFromString (" 19 ", vars, null, Nil, XPathConstants.STRING)
    assertEquals(StringResult("19"), result)
    result = XPathUtil evalExpressionFromString (" 19 * 47 ", vars, null, Nil, XPathConstants.STRING)
    assertEquals(StringResult("893"), result)
    result = XPathUtil evalExpressionFromString (" '' ", vars, null, Nil, XPathConstants.STRING)
    assertEquals(StringResult(""), result)
  }
}
