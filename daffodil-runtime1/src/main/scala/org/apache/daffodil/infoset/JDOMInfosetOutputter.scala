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

class JDOMInfosetOutputter extends InfosetOutputter
    with XMLInfosetOutputter {

  private val stack = new MStackOf[org.jdom2.Parent]
  private var result: Maybe[org.jdom2.Document] = Maybe.Nope
  private val xsiNS = org.jdom2.Namespace.getNamespace("xsi", XMLUtils.XSI_NAMESPACE.toString)

  def reset(): Unit = {// call to reuse these. When first constructed no reset call is necessary.
    result = Maybe.Nope
    stack.clear
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

  def startSimple(diSimple: DISimple): Unit = {

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
  }

  def endSimple(diSimple: DISimple): Unit = {
  }

  def startComplex(diComplex: DIComplex): Unit = {

    val elem = createElement(diComplex)

    stack.top.addContent(elem)
    stack.push(elem)
  }

  def endComplex(diComplex: DIComplex): Unit = {
    stack.pop
  }

  def startArray(diArray: DIArray): Unit = {
  }
  def endArray(diArray: DIArray): Unit = {
  }

  def getResult(): org.jdom2.Document = {
    Assert.usage(result.isDefined, "No result to get. Must check isError parse result before calling getResult")
    result.get
  }

  private def createElement(diElement: DIElement): org.jdom2.Element = {
    val elem: org.jdom2.Element =
      if(diElement.erd.namedQName.namespace.isNoNamespace)
        new org.jdom2.Element(diElement.erd.name)
      else
        new org.jdom2.Element(diElement.erd.name, diElement.erd.prefix,
          diElement.erd.namedQName.namespace)

    if (isNilled(diElement)) {
      elem.setAttribute("nil", "true", xsiNS)
    }
    elem
  }

}

