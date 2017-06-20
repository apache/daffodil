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

class JDOMInfosetOutputter extends InfosetOutputter
    with XMLInfosetOutputter {

  private val stack = new MStackOf[org.jdom2.Parent]
  private var result: Maybe[org.jdom2.Document] = Maybe.Nope
  private val xsiNS = org.jdom2.Namespace.getNamespace("xsi", XMLUtils.XSI_NAMESPACE.toString)

  def reset(): Unit = {// call to reuse these. When first constructed no reset call is necessary.
    result = Maybe.Nope
    stack.clear
  }

  def startDocument(): Boolean = {
    stack.push(new org.jdom2.Document)
    true
  }

  def endDocument(): Boolean = {
    val root = stack.pop
    assert(stack.isEmpty)
    assert(root.isInstanceOf[org.jdom2.Document])
    result = Maybe(root.asInstanceOf[org.jdom2.Document])
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
      elem.addContent(text)
    }

    stack.top.addContent(elem)

    true
  }

  def endSimple(diSimple: DISimple): Boolean = {
    true
  }

  def startComplex(diComplex: DIComplex): Boolean = {

    val elem = createElement(diComplex)

    stack.top.addContent(elem)
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

  def getResult(): org.jdom2.Document = {
    Assert.usage(result.isDefined, "No result to get. Must check isError parse result before calling getResult")
    result.get
  }

  private def createElement(diElement: DIElement): org.jdom2.Element = {
    val elem: org.jdom2.Element =
      if(diElement.erd.thisElementsNamespace.isNoNamespace)
        new org.jdom2.Element(diElement.erd.name)
      else
        new org.jdom2.Element(diElement.erd.name, diElement.erd.thisElementsNamespacePrefix,
          diElement.erd.thisElementsNamespace)

    if (diElement.isNilled && diElement.erd.isNillable) {
      elem.setAttribute("nil", "true", xsiNS)
    }
    elem
  }

}

