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

import scala.collection.mutable.ListBuffer
import scala.xml.MetaData
import scala.xml.Null
import scala.xml.UnprefixedAttribute

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.xml.XMLUtils


class ScalaXMLInfosetOutputter(showFormatInfo: Boolean = false, showFreedInfo: Boolean = false) extends InfosetOutputter
    with XMLInfosetOutputter {

  protected val stack = new MStackOf[ListBuffer[scala.xml.Node]]
  private var resultNode: Maybe[scala.xml.Node] = Maybe.Nope

  def reset(): Unit = {// call to reuse these. When first constructed no reset call is necessary.
    resultNode = Maybe.Nope
    stack.clear
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
    val nilAttr = if (isNilled(diElem)) XMLUtils.xmlNilAttribute else Null
    val freedAttr =
      if (showFreedInfo) {
        val selfFreed = diElem.wouldHaveBeenFreed
        val arrayFreed =
          if (diElem.erd.isArray) diElem.diParent.children.find { _.erd eq diElem.erd }.get.wouldHaveBeenFreed
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

  def startSimple(diSimple: DISimple): Unit = {

    val attributes = getAttributes(diSimple)

    val children =
      if (!isNilled(diSimple) && diSimple.hasValue) {
        val text =
          if (diSimple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
            remapped(diSimple.dataValueAsString)
          } else {
            diSimple.dataValueAsString
          }
        Seq(new scala.xml.Text(text))
      } else {
        Seq()
      }

    val elem =
      scala.xml.Elem(
        diSimple.erd.prefix,
        diSimple.erd.name,
        attributes,
        diSimple.erd.minimizedScope,
        minimizeEmpty = true,
        children: _*)

    val elemWithFmt = addFmtInfo(diSimple, elem, showFormatInfo)
    stack.top.append(elemWithFmt)
  }

  def endSimple(diSimple: DISimple): Unit = {
  }

  def startComplex(diComplex: DIComplex): Unit = {
    stack.push(new ListBuffer())
  }

  def endComplex(diComplex: DIComplex): Unit = {

    val attributes = getAttributes(diComplex)
    val children = stack.pop

    val elem =
      scala.xml.Elem(
        diComplex.erd.prefix,
        diComplex.erd.name,
        attributes,
        diComplex.erd.minimizedScope,
        minimizeEmpty = true,
        children: _*)

    val elemWithFmt = addFmtInfo(diComplex, elem, showFormatInfo)
    stack.top.append(elemWithFmt)
  }

  def startArray(diArray: DIArray): Unit = {
    // Array elements are started individually
  }
  def endArray(diArray: DIArray): Unit = {
  }

  def getResult(): scala.xml.Node = {
    Assert.usage(resultNode.isDefined, "No result to get. Must check isError parse result before calling getResult")
    resultNode.get
  }
}
