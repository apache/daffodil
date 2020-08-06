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
import scala.xml.Null
import scala.collection.mutable.ListBuffer
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dpath.NodeInfo

class ScalaXMLInfosetOutputter(showFormatInfo: Boolean = false) extends InfosetOutputter
    with XMLInfosetOutputter {

  protected val stack = new MStackOf[ListBuffer[scala.xml.Node]]
  private var resultNode: Maybe[scala.xml.Node] = Maybe.Nope

  def reset(): Unit = {// call to reuse these. When first constructed no reset call is necessary.
    resultNode = Maybe.Nope
    stack.clear
  }

  def startDocument(): Boolean = {
    stack.push(new ListBuffer())
    true
  }

  def endDocument(): Boolean = {
    val root = stack.pop
    assert(root.length == 1)
    resultNode = Maybe(root(0))
    true
  }

  def startSimple(diSimple: DISimple): Boolean = {

    val e =
      if (isNilled(diSimple)) {
        scala.xml.Elem(diSimple.erd.namedQName.prefixOrNull, diSimple.erd.name,
            XMLUtils.xmlNilAttribute, diSimple.erd.minimizedScope, minimizeEmpty = true)
      } else if (diSimple.hasValue) {
        val text =
          if (diSimple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
            remapped(diSimple.dataValueAsString)
          } else {
            diSimple.dataValueAsString
          }
        val textNode = new scala.xml.Text(text)
        scala.xml.Elem(diSimple.erd.namedQName.prefixOrNull, diSimple.erd.name, Null,
          diSimple.erd.minimizedScope, minimizeEmpty = true, textNode)
      } else {
        // element has been created but has no value yet, display an empty element tag
        scala.xml.Elem(diSimple.erd.namedQName.prefixOrNull, diSimple.erd.name, Null,
            diSimple.erd.minimizedScope, minimizeEmpty = true)
      }

    val elem = addFmtInfo(diSimple, e, showFormatInfo)

    stack.top.append(elem)
    //returning true/false will be used when recursion is removed
    true
  }

  def endSimple(diSimple: DISimple): Boolean = {
    true
  }

  def startComplex(diComplex: DIComplex): Boolean = {
    stack.push(new ListBuffer())
    true
  }

  def endComplex(diComplex: DIComplex): Boolean = {

    val children = stack.pop

    val e =
      if (isNilled(diComplex)) {
        scala.xml.Elem(diComplex.erd.namedQName.prefixOrNull, diComplex.erd.name,
          XMLUtils.xmlNilAttribute, diComplex.erd.minimizedScope, minimizeEmpty = true)
      } else {
        scala.xml.Elem(diComplex.erd.namedQName.prefixOrNull, diComplex.erd.name,
            scala.xml.Null, diComplex.erd.minimizedScope,  minimizeEmpty = true, children: _*)
      }

    val elem = addFmtInfo(diComplex, e, showFormatInfo)

    stack.top.append(elem)
    //returning true/false will be used when recursion is removed
    true
  }

  def startArray(diArray: DIArray): Boolean = {
    // Array elements are started individually
    true
  }
  def endArray(diArray: DIArray): Boolean = {
    true
  }

  def getResult(): scala.xml.Node = {
    Assert.usage(resultNode.isDefined, "No result to get. Must check isError parse result before calling getResult")
    resultNode.get
  }
}
