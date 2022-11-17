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

import java.io.StringReader
import java.nio.charset.StandardCharsets
import javax.xml.stream.XMLStreamConstants._

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Indentable

/**
 * Writes the infoset to a java.io.Writer as XML text.
 *
 * @param writer The writer to write the XML text to
 * @param pretty Whether or to enable pretty printing. Set to true, XML
 *               elements are indented and newlines are inserted.
 */
class XMLTextInfosetOutputter private (writer: java.io.Writer, pretty: Boolean)
  extends InfosetOutputter with Indentable with XMLInfosetOutputter {

  def this(os: java.io.OutputStream, pretty: Boolean) = {
    this(new java.io.OutputStreamWriter(os, StandardCharsets.UTF_8), pretty)
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

  private def writeStringAsXml(str: String): Unit = {
    // create a wrapper element that allows us to reset the default XML
    // namespace. This ensures the embedded XML does not inherit the default
    // namespaces if one is defined in the infoset
    incrementIndentation()
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    writer.write("<")
    writer.write(XMLTextInfoset.stringAsXml)
    writer.write(" xmlns=\"\">")

    if (pretty) {
      writer.write(System.lineSeparator())
    }

    // Parse the string as XML and then write all events out to the
    // XMLStreamWriter. This performs basic validation of the XML so we do not
    // create an invalid infoset, though it may contain XML features that
    // aren't normally in an XML infoset (e.g. elements with attributes). This
    // logic also skips the START_DOCUMENT event so that the XML declaration is
    // not written in the middle of our XML infoset
    val sr = new StringReader(str)
    val xsr = XMLTextInfoset.xmlInputFactory.createXMLStreamReader(sr)
    val xsw = XMLTextInfoset.xmlOutputFactory.createXMLStreamWriter(writer, StandardCharsets.UTF_8.toString)
    Assert.invariant(xsr.getEventType() == START_DOCUMENT)
    while (xsr.hasNext()) {
      xsr.next()
      XMLTextInfoset.writeXMLStreamEvent(xsr, xsw)
    }

    // write the closing wrapper element
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    writer.write("</")
    writer.write(XMLTextInfoset.stringAsXml)
    writer.write(">")
    decrementIndentation()

    // if pretty, write indentation so that the closing tag of the simple
    // element is indented as if it were complex
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
  }

  override def startSimple(simple: DISimple): Unit = {
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    outputStartTag(simple)

    if (!isNilled(simple) && simple.hasValue) {
      if (simple.erd.optPrimType.get == NodeInfo.String) {
        val simpleVal = simple.dataValueAsString
        if (simple.erd.runtimeProperties.get(XMLTextInfoset.stringAsXml) == "true") {
          writeStringAsXml(simpleVal)
        } else {
          writer.write(scala.xml.Utility.escape(remapped(simpleVal)))
        }
      } else {
        writer.write(simple.dataValueAsString)
      }
    }

    outputEndTag(simple)
    inScopeComplexElementHasChildren = true
  }
  
  override def endSimple(simple: DISimple): Unit = {
    // do nothing, everything is done in startSimple
  }

  override def startComplex(complex: DIComplex): Unit = {
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    outputStartTag(complex)
    incrementIndentation()
    inScopeComplexElementHasChildren = false
  }

  override def endComplex(complex: DIComplex): Unit = {
    decrementIndentation()
    if (pretty && inScopeComplexElementHasChildren) {
      // only output newline and indentation for non-empty complex types
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    outputEndTag(complex)
    inScopeComplexElementHasChildren = true
  }

  override def startArray(array: DIArray): Unit = {
    // do nothing
  }

  override def endArray(array: DIArray): Unit = {
    // do nothing
  }

  override def startDocument(): Unit = {
    writer.write("""<?xml version="1.0" encoding="UTF-8"?>""")
  }

  override def endDocument(): Unit = {
    writer.write(System.lineSeparator())
    writer.flush()
  }
}
