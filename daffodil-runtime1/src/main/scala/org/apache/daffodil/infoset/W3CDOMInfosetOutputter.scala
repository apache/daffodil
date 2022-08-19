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

package org.apache.daffodil.infoset

import org.apache.daffodil.util.Maybe
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dpath.NodeInfo
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

  def startDocument(): Unit = {
    val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true)
    document = factory.newDocumentBuilder().newDocument()
    stack.push(document)
  }

  def endDocument(): Unit = {
    val root = stack.pop
    assert(stack.isEmpty)
    assert(root.isInstanceOf[Document])
    result = Maybe(root.asInstanceOf[Document])
  }

  def startSimple(diSimple: DISimple): Unit = {

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
  }

  def endSimple(diSimple: DISimple): Unit = {
  }

  def startComplex(diComplex: DIComplex): Unit = {

    val elem = createElement(diComplex)
    stack.top.appendChild(elem)
    stack.push(elem)
  }

  def endComplex(diComplex: DIComplex): Unit = {
    stack.pop
  }

  def startArray(diArray: DIArray): Unit = {
  }
  def endArray(diArray: DIArray): Unit = {
  }

  def getResult(): Document = {
    Assert.usage(result.isDefined, "No result to get. Must check isError parse result before calling getResult")
    result.get
  }

  private def createElement(diElement: DIElement): Element = {

    assert(document != null)

    val elem: Element =
      if(diElement.erd.namedQName.namespace.isNoNamespace) {
        document.createElementNS(null, diElement.erd.name)
      } else {
        document.createElementNS(diElement.erd.namedQName.namespace, diElement.erd.prefixedName)
      }

    if (isNilled(diElement)) {
      elem.setAttributeNS(XMLUtils.XSI_NAMESPACE.toString, "xsi:nil", "true")
    }

    elem
  }

}

