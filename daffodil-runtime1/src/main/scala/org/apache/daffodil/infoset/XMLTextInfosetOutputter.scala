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

import org.apache.daffodil.util.Indentable
import org.apache.daffodil.dpath.NodeInfo

/**
 * Writes the infoset to a java.io.Writer as XML text.
 *
 * @param writer The writer to write the XML text to
 * @param pretty Whether or to enable pretty printing. Set to true, XML
 *               elements are indented and newlines are inserted.
 */
class XMLTextInfosetOutputter(writer: java.io.Writer, pretty: Boolean = true)
  extends InfosetOutputter with Indentable with XMLInfosetOutputter {

  private val sb = new StringBuilder()

  override def reset(): Unit = {
    resetIndentation()
  }

  private def getTagName(elem: DIElement): String = {
    val prefix = elem.erd.thisElementsNamespacePrefix
    if (prefix == null || prefix == "") elem.erd.name else prefix + ":" + elem.erd.name
  }

  private def outputStartTag(elem: DIElement, name: String): Unit = {
    writer.write("<")

    writer.write(name)

    val nsbStart = elem.erd.minimizedScope
    val nsbEnd = if (elem.isRoot) scala.xml.TopScope else elem.diParent.erd.minimizedScope
    if (nsbStart != nsbEnd) {
      sb.setLength(0) // reset the stringbuilder
      nsbStart.buildString(sb, nsbEnd)
      writer.write(sb.toString)
    }

    if (isNilled(elem)) {
      writer.write(" xsi:nil=\"true\"")
    }

    writer.write(">")
  }

  private def outputEndTag(elem: DIElement, name: String): Unit = {
    writer.write("</")
    writer.write(name)
    writer.write(">")
  }

  override def startSimple(simple: DISimple): Boolean = {
    if (pretty) outputIndentation(writer)
    val name = getTagName(simple)
    outputStartTag(simple, name)

    if (!isNilled(simple) && simple.hasValue) {
      val text =
        if (simple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
          val s = remapped(simple.dataValueAsString)
          scala.xml.Utility.escape(s)
        } else {
          simple.dataValueAsString
        }

      writer.write(text)
    }

    outputEndTag(simple, name)
    if (pretty) writer.write(System.lineSeparator())
    true
  }
  
  override def endSimple(simple: DISimple): Boolean = {
    // do nothing, everything is done in startSimple
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    val name = getTagName(complex)
    if (pretty) outputIndentation(writer)
    outputStartTag(complex, name)
    incrementIndentation()
    if (pretty) writer.write(System.lineSeparator())
    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    val name = getTagName(complex)
    decrementIndentation()
    if (pretty) outputIndentation(writer)
    outputEndTag(complex, name)
    if (pretty) writer.write(System.lineSeparator())
    true
  }

  override def startArray(array: DIArray): Boolean = {
    // do nothing
    true
  }

  override def endArray(array: DIArray): Boolean = {
    // do nothing
    true
  }

  override def startDocument(): Boolean = {
    writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>")
    if (pretty) writer.write(System.lineSeparator())
    true
  }

  override def endDocument(): Boolean = {
    writer.flush()
    true
  }
}
