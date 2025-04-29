/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.api.infoset.{ JDOMInfosetOutputter => JJDOMInfosetOutputter }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDLPrimType
import org.apache.daffodil.runtime1.iapi.InfosetArray
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement
import org.apache.daffodil.runtime1.iapi.InfosetElement
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement

class JDOMInfosetOutputter extends JJDOMInfosetOutputter {

  private val stack = new MStackOf[org.jdom2.Parent]
  private var result: Maybe[org.jdom2.Document] = Maybe.Nope
  private val xsiNS = org.jdom2.Namespace.getNamespace("xsi", XMLUtils.XSI_NAMESPACE.toString)

  def reset()
    : Unit = { // call to reuse these. When first constructed no reset call is necessary.
    result = Maybe.Nope
    stack.clear()
  }

  def startDocument(): Unit = {
    stack.push(new org.jdom2.Document)
  }

  def endDocument(): Unit = {
    val root = stack.pop
    assert(stack.isEmpty)
    assert(root.isInstanceOf[org.jdom2.Document])
    result = Maybe(root.asInstanceOf[org.jdom2.Document])
  }

  override def startSimple(simple: InfosetSimpleElement): Unit = {

    val elem = createElement(simple)

    if (!simple.isNilled) {
      val text =
        if (simple.metadata.dfdlType == DFDLPrimType.String) {
          XMLUtils.remapXMLIllegalCharactersToPUA(simple.getText)
        } else {
          simple.getText
        }
      elem.addContent(text)
    }

    stack.top.addContent(elem)
  }

  override def endSimple(se: InfosetSimpleElement): Unit = {}

  override def startComplex(ce: InfosetComplexElement): Unit = {

    val elem = createElement(ce)

    stack.top.addContent(elem)
    stack.push(elem)
  }

  override def endComplex(ce: InfosetComplexElement): Unit = {
    stack.pop
  }

  override def startArray(ar: InfosetArray): Unit = {}
  override def endArray(ar: InfosetArray): Unit = {}

  def getResult(): org.jdom2.Document = {
    Assert.usage(
      result.isDefined,
      "No result to get. Must check isError parse result before calling getResult"
    )
    result.get
  }

  private def createElement(element: InfosetElement): org.jdom2.Element = {
    val elem: org.jdom2.Element =
      if (element.metadata.namespace eq null)
        new org.jdom2.Element(element.metadata.name)
      else
        new org.jdom2.Element(
          element.metadata.name,
          element.metadata.prefix,
          element.metadata.namespace
        )

    if (element.isNilled) {
      elem.setAttribute("nil", "true", xsiNS)
    }
    elem
  }

}
