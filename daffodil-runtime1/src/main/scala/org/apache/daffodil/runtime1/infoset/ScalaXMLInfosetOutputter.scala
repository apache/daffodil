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

import scala.collection.mutable.ListBuffer
import scala.xml.MetaData
import scala.xml.Null
import scala.xml.UnprefixedAttribute

import org.apache.daffodil.api.infoset.{ ScalaXMLInfosetOutputter => JScalaXMLInfosetOutputter }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDLPrimType
import org.apache.daffodil.runtime1.iapi.InfosetArray
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement

class ScalaXMLInfosetOutputter(showFreedInfo: Boolean = false)
  extends JScalaXMLInfosetOutputter {

  protected val stack = new MStackOf[ListBuffer[scala.xml.Node]]
  private var resultNode: Maybe[scala.xml.Node] = Maybe.Nope

  def reset()
    : Unit = { // call to reuse these. When first constructed no reset call is necessary.
    resultNode = Maybe.Nope
    stack.clear()
  }

  def startDocument(): Unit = {
    stack.push(new ListBuffer())
  }

  def endDocument(): Unit = {
    val root = stack.pop
    assert(root.length == 1)
    resultNode = Maybe(root(0))
  }

  private def getAttributes(diElem: DIElement): MetaData = {
    val nilAttr = if (diElem.isNilled) XMLUtils.xmlNilAttribute else Null
    val freedAttr =
      if (showFreedInfo) {
        val selfFreed = diElem.wouldHaveBeenFreed
        val arrayFreed =
          if (diElem.erd.isArray)
            diElem.diParent
              .find {
                _.erd eq diElem.erd
              }
              .get
              .wouldHaveBeenFreed
          else false
        if (selfFreed || arrayFreed) {
          val freedAttrVal =
            if (selfFreed && arrayFreed) "self+array"
            else if (selfFreed) "self"
            else "array"
          new UnprefixedAttribute("freed", freedAttrVal, nilAttr)
        } else {
          nilAttr
        }
      } else {
        nilAttr
      }
    freedAttr
  }

  override def startSimple(se: InfosetSimpleElement): Unit = {
    val diSimple = se.asInstanceOf[DISimple]
    val attributes = getAttributes(diSimple)

    val children =
      if (!diSimple.isNilled && diSimple.hasValue) {
        val text =
          if (diSimple.metadata.dfdlType == DFDLPrimType.String) {
            XMLUtils.remapXMLIllegalCharactersToPUA(diSimple.dataValueAsString)
          } else {
            diSimple.dataValueAsString
          }
        Seq(new scala.xml.Text(text))
      } else {
        Seq()
      }

    val elem =
      scala.xml.Elem(
        diSimple.metadata.prefix,
        diSimple.metadata.name,
        attributes,
        diSimple.metadata.minimizedScope,
        minimizeEmpty = true,
        children: _*
      )

    stack.top.append(elem)
  }

  override def endSimple(se: InfosetSimpleElement): Unit = {}

  override def startComplex(ce: InfosetComplexElement): Unit = {
    stack.push(new ListBuffer())
  }

  override def endComplex(ce: InfosetComplexElement): Unit = {

    val diComplex = ce.asInstanceOf[DIComplex]
    val attributes = getAttributes(diComplex)
    val children = stack.pop

    val elem =
      scala.xml.Elem(
        diComplex.metadata.prefix,
        diComplex.metadata.name,
        attributes,
        diComplex.metadata.minimizedScope,
        minimizeEmpty = true,
        children: _*
      )

    stack.top.append(elem)
  }

  override def startArray(ar: InfosetArray): Unit = {
    // Array elements are started individually
  }
  def endArray(ar: InfosetArray): Unit = {}

  def getResult(): scala.xml.Node = {
    Assert.usage(
      resultNode.isDefined,
      "No result to get. Must check isError parse result before calling getResult"
    )
    resultNode.get
  }
}
