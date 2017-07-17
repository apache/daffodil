/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import javax.xml.parsers.DocumentBuilderFactory; 

class W3CDOMInfosetOutputter extends InfosetOutputter
    with XMLInfosetOutputter {

  private var document: Document = null
  private val stack = new MStackOf[Node]
  private var result: Maybe[Document] = Maybe.Nope

  def reset(): Unit = {// call to reuse these. When first constructed no reset call is necessary.
    result = Maybe.Nope
    document = null
    stack.clear
  }

  def startDocument(): Boolean = {
    val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true)
    document = factory.newDocumentBuilder().newDocument()
    stack.push(document)
    true
  }

  def endDocument(): Boolean = {
    val root = stack.pop
    assert(stack.isEmpty)
    assert(root.isInstanceOf[Document])
    result = Maybe(root.asInstanceOf[Document])
    true
  }

  def startSimple(diSimple: DISimple): Boolean = {

    val elem = createElement(diSimple)

    if (diSimple.hasValue) {
      val text =
        if (diSimple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
          remapped(diSimple.dataValueAsString)
        } else {
          diSimple.dataValueAsString
        }
      elem.appendChild(document.createTextNode(text))
    }

    stack.top.appendChild(elem)

    true
  }

  def endSimple(diSimple: DISimple): Boolean = {
    true
  }

  def startComplex(diComplex: DIComplex): Boolean = {

    val elem = createElement(diComplex)
    stack.top.appendChild(elem)
    stack.push(elem)
    true
  }

  def endComplex(diComplex: DIComplex): Boolean = {
    stack.pop
    true
  }

  def startArray(diArray: DIArray): Boolean = {
    true
  }
  def endArray(diArray: DIArray): Boolean = {
    true
  }

  def getResult(): Document = {
    Assert.usage(result.isDefined, "No result to get. Must check isError parse result before calling getResult")
    result.get
  }

  private def createElement(diElement: DIElement): Element = {

    assert(document != null)

    val elem: Element =
      if(diElement.erd.thisElementsNamespace.isNoNamespace) {
        document.createElementNS(null, diElement.erd.name)
      } else {
        document.createElementNS(diElement.erd.thisElementsNamespace, diElement.erd.prefixedName)
      }

    if (diElement.isNilled && diElement.erd.isNillable) {
      elem.setAttributeNS(XMLUtils.XSI_NAMESPACE.toString, "xsi:nil", "true")
    }

    elem
  }

}

