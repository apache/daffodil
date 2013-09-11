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


import org.jdom2.Document
import org.jdom2.Element
import org.jdom2.Text
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.xml._
import junit.framework.Assert._
import org.junit.Test

class XPathUtilTest2 {

  /**
   * Test that uses default namespace, not no-namespace.
   */
  @Test def testXPathWithDefaultNS1() {

    // <root xmlns="urn:foobarNS><child1>19</child1></root>
    val text = new Text("19")
    val child1 = new Element("child1", "urn:foobarNS")
    val root = new Element("root", "urn:foobarNS")
    child1 addContent (text)
    root addContent (child1)
    val document = new Document(root)

    val ns = List(org.jdom2.Namespace.getNamespace("", "urn:foobarNS"))
    val result = XPathUtil evalExpressionFromString ("/root/child1",
      new VariableMap(),
      root,
      ns)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        println(result)
        fail
      }
    }
  }

  @Test def testXPathWithNoNamespace() {

    // <root xmlns="urn:foobarNS><child1>19</child1></root>
    val text = new Text("19")
    val child1 = new Element("child1")
    val root = new Element("root")
    child1 addContent (text)
    root addContent (child1)
    val document = new Document(root)

    val ns = Nil // List(org.jdom2.Namespace.getNamespace("", "urn:foobarNS"))
    val result = XPathUtil evalExpressionFromString ("/root/child1",
      new VariableMap(),
      root,
      ns)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        println(result)
        fail
      }
    }
  }

  /**
   * Variation just gets the namespace by way of the root.getNamespace() call.
   */
  @Test def testXPathWithDefaultNS2() {

    // <root xmlns="urn:foobarNS><child1>19</child1></root>
    val text = new Text("19")
    val child1 = new Element("child1", "urn:foobarNS")
    val root = new Element("root", "urn:foobarNS")
    child1 addContent (text)
    root addContent (child1)
    val document = new Document(root)

    val ns = List(root.getNamespace()) // different way to get namespace info.
    val result = XPathUtil evalExpressionFromString ("/root/child1",
      new VariableMap(),
      root,
      ns)

    result match {
      case NodeResult(x) => assertEquals("19", x.getText())
      case _ => {
        println(result)
        fail
      }
    }
  }

}
