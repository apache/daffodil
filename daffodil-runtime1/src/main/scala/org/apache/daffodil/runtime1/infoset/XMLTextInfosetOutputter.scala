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

import java.io.StringReader
import java.nio.charset.StandardCharsets
import javax.xml.stream.XMLStreamConstants._

import org.apache.daffodil.api.infoset.{ InfosetOutputter => JInfosetOutputter }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Indentable
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.iapi.InfosetArray
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement

/**
 * Writes the infoset to a java.io.BufferedWriter as XML text.
 *
 * @param writer             The writer to write the XML text to
 * @param pretty             Whether or to enable pretty printing. Set to true, XML
 *                           elements are indented and newlines are inserted.
 * @param xmlTextEscapeStyle Determine whether to wrap values of elements of type
 *                           xs:string in CDATA tags in order to preserve whitespace.
 * @param minimal            Determine whether to exclude xml slug and prefix bindings
 */
class XMLTextInfosetOutputter private (
  writer: java.io.BufferedWriter,
  pretty: Boolean,
  xmlTextEscapeStyle: XMLTextEscapeStyle.Value,
  minimal: Boolean
) extends JInfosetOutputter
  with Indentable {

  def this(
    os: java.io.OutputStream,
    pretty: Boolean,
    xmlTextEscapeStyle: XMLTextEscapeStyle.Value = XMLTextEscapeStyle.Standard,
    minimal: Boolean = false
  ) = {
    // using a BufferedWriter provides significant performance improvements
    this(
      new java.io.BufferedWriter(new java.io.OutputStreamWriter(os, StandardCharsets.UTF_8)),
      pretty,
      xmlTextEscapeStyle,
      minimal
    )
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

    if (!minimal) {
      val nsbStart = elem.erd.minimizedScope
      val nsbEnd = if (elem.isRoot) scala.xml.TopScope else elem.diParent.erd.minimizedScope
      if (nsbStart != nsbEnd) {
        sb.setLength(0) // reset the stringbuilder
        nsbStart.buildString(sb, nsbEnd)
        writer.write(sb.toString)
      }
    }

    if (elem.isNilled) {
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
      writer.newLine()
      outputIndentation(writer)
    }
    writer.write("<")
    writer.write(XMLTextInfoset.stringAsXml)
    writer.write(" xmlns=\"\">")

    if (pretty) {
      writer.newLine()
    }

    // Parse the string as XML and then write all events out to the
    // XMLStreamWriter. This performs basic validation of the XML so we do not
    // create an invalid infoset, though it may contain XML features that
    // aren't normally in an XML infoset (e.g. elements with attributes). This
    // logic also skips the START_DOCUMENT event so that the XML declaration is
    // not written in the middle of our XML infoset
    val sr = new StringReader(str)
    val xsr = XMLTextInfoset.xmlInputFactory.createXMLStreamReader(sr)
    val xsw = XMLTextInfoset.xmlOutputFactory.createXMLStreamWriter(
      writer,
      StandardCharsets.UTF_8.toString
    )
    Assert.invariant(xsr.getEventType() == START_DOCUMENT)
    while (xsr.hasNext()) {
      xsr.next()
      XMLTextInfoset.writeXMLStreamEvent(xsr, xsw)
    }

    // write the closing wrapper element
    if (pretty) {
      writer.newLine()
      outputIndentation(writer)
    }
    writer.write("</")
    writer.write(XMLTextInfoset.stringAsXml)
    writer.write(">")
    decrementIndentation()

    // if pretty, write indentation so that the closing tag of the simple
    // element is indented as if it were complex
    if (pretty) {
      writer.newLine()
      outputIndentation(writer)
    }
  }

  override def startSimple(se: InfosetSimpleElement): Unit = {
    val simple = se.asInstanceOf[DISimple]
    if (pretty) {
      writer.newLine()
      outputIndentation(writer)
    }
    outputStartTag(simple)

    if (simple.hasValue) {
      if (simple.erd.optPrimType.get == NodeInfo.String) {
        val simpleVal = simple.dataValueAsString
        if (simple.erd.runtimeProperties.get(XMLTextInfoset.stringAsXml) == "true") {
          writeStringAsXml(simpleVal)
        } else {
          val xmlSafe = XMLUtils.remapXMLIllegalCharactersToPUA(simpleVal)
          val escaped = xmlTextEscapeStyle match {
            case XMLTextEscapeStyle.CDATA => {
              val needsCDataEscape = xmlSafe.exists { c =>
                c == '<' || c == '>' || c == '"' || c == '&' || c.isWhitespace
              }
              if (needsCDataEscape) {
                "<![CDATA[%s]]>".format(xmlSafe.replaceAll("]]>", "]]]]><![CDATA[>"))
              } else {
                xmlSafe
              }
            }
            case XMLTextEscapeStyle.Standard => {
              val needsStandardEscape = xmlSafe.exists { c =>
                c == '<' || c == '>' || c == '"' || c == '&'
              }
              if (needsStandardEscape) {
                scala.xml.Utility.escape(xmlSafe)
              } else {
                xmlSafe
              }
            }
          }
          writer.write(escaped)
        }
      } else {
        writer.write(simple.dataValueAsString)
      }
    }

    outputEndTag(simple)
    inScopeComplexElementHasChildren = true
  }

  override def endSimple(simple: InfosetSimpleElement): Unit = {
    // do nothing, everything is done in startSimple
  }

  override def startComplex(ce: InfosetComplexElement): Unit = {
    val complex = ce.asInstanceOf[DIComplex]
    if (pretty) {
      writer.newLine()
      outputIndentation(writer)
    }
    outputStartTag(complex)
    incrementIndentation()
    inScopeComplexElementHasChildren = false
  }

  override def endComplex(ce: InfosetComplexElement): Unit = {
    val complex = ce.asInstanceOf[DIComplex]
    decrementIndentation()
    if (pretty && inScopeComplexElementHasChildren) {
      // only output newline and indentation for non-empty complex types
      writer.newLine()
      outputIndentation(writer)
    }
    outputEndTag(complex)
    inScopeComplexElementHasChildren = true
  }

  override def startArray(array: InfosetArray): Unit = {
    // do nothing
  }

  override def endArray(array: InfosetArray): Unit = {
    // do nothing
  }

  override def startDocument(): Unit = {
    if (!minimal) {
      writer.write("""<?xml version="1.0" encoding="UTF-8"?>""")
    }
  }

  override def endDocument(): Unit = {
    writer.newLine()
    writer.flush()
  }
}
