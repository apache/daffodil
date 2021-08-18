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

import java.nio.charset.Charset

import org.apache.daffodil.util.Indentable
import org.apache.daffodil.dpath.NodeInfo

/**
 * Writes the infoset to a java.io.Writer as XML text.
 *
 * @param writer The writer to write the XML text to
 * @param pretty Whether or to enable pretty printing. Set to true, XML
 *               elements are indented and newlines are inserted.
 */
class XMLTextInfosetOutputter private (writer: java.io.Writer, pretty: Boolean, dummy: Int)
  extends InfosetOutputter with Indentable with XMLInfosetOutputter {

  @deprecated("This constructor is deprecated. Use XMLTextInfosetOutputter(java.io.OutputStream, Boolean) instead.", "2.4.0")
  def this(writer: java.io.Writer, pretty: Boolean = true) = {
    this(writer, pretty, 0)
  }

  def this(os: java.io.OutputStream, pretty: Boolean) = {
    this(new java.io.OutputStreamWriter(os, Charset.forName("UTF-8")), pretty, 0)
  }

  private val sb = new StringBuilder()

  /**
   * Used to keep determine if the in-scope complex element has children, and
   * thus if we should output a newline or not when closing that complex type.
   * A complex type with no children should just be output like
   *
   *   <complex></complex>
   *
   * This value is initialized to false when a complex type is started, since
   * we don't know if it has children yet. This value is then set to true
   * either when a simple type is started (i.e. the current complex type must
   * have at least one child), or when a complex type is ended (i.e. the parent
   * and all subsequent parents of the ended complex must have at least one
   * child, which is the complex that just eneded).
   */
  private var inScopeComplexElementHasChildren = false

  override def reset(): Unit = {
    resetIndentation()
    inScopeComplexElementHasChildren = false
  }

  private def outputTagName(elem: DIElement): Unit = {
    val prefix = elem.erd.prefix
    if (prefix != null && prefix != "") {
      writer.write(prefix)
      writer.write(":")
    }
    writer.write(elem.erd.name)
  }

  private def outputStartTag(elem: DIElement): Unit = {
    writer.write("<")

    outputTagName(elem)

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

  private def outputEndTag(elem: DIElement): Unit = {
    writer.write("</")
    outputTagName(elem)
    writer.write(">")
  }

  override def startSimple(simple: DISimple): Boolean = {
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    outputStartTag(simple)

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

    outputEndTag(simple)
    inScopeComplexElementHasChildren = true

    true
  }
  
  override def endSimple(simple: DISimple): Boolean = {
    // do nothing, everything is done in startSimple
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    outputStartTag(complex)
    incrementIndentation()
    inScopeComplexElementHasChildren = false

    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    decrementIndentation()
    if (pretty && inScopeComplexElementHasChildren) {
      // only output newline and indentation for non-empty complex types
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    outputEndTag(complex)
    inScopeComplexElementHasChildren = true

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
    writer.write("""<?xml version="1.0" encoding="UTF-8"?>""")
    true
  }

  override def endDocument(): Boolean = {
    writer.write(System.lineSeparator())
    writer.flush()
    true
  }
}
