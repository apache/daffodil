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

import java.io.OutputStream
import java.io.OutputStreamWriter

import scala.xml.NamespaceBinding

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.util.Indentable
import org.apache.daffodil.util.MStackOfBoolean
import org.apache.daffodil.xml.XMLUtils
import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.Locator
import org.xml.sax.SAXException
import org.xml.sax.helpers.AttributesImpl

class SAXInfosetOutputter(xmlReader: DFDL.DaffodilXMLReader)
  extends InfosetOutputter
  with XMLInfosetOutputter {
  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * inbetween calls to the parse method.
   */
  override def reset(): Unit = {
    // this doesn't do anything as the ContentHandler API does not support
    // resetting, but some implemented ContentHandlers, such as the JDOM SaxHandler,
    // do support resetting so it's up to the creator of the contentHandler, to call
    // their contentHandler's reset if applicable and if necessary
  }

  override def startDocument(): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    try {
      contentHandler.startDocument()
      true
    } catch {
      case _: SAXException => false
    }
  }

  override def endDocument(): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    try {
      contentHandler.endDocument()
      true
    } catch {
      case _: SAXException => false
    }
  }

  override def startSimple(diSimple: DISimple): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    try {
      doStartElement(diSimple, contentHandler)
      if (diSimple.hasValue) {
        val text =
          if (diSimple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
            val s = remapped(diSimple.dataValueAsString)
            scala.xml.Utility.escape(s)
          } else {
            diSimple.dataValueAsString
          }
        val arr = text.toCharArray
        contentHandler.characters(arr, 0, arr.length)
      }
      true
    } catch {
      case _: SAXException => false
    }
  }

  override def endSimple(diSimple: DISimple): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    try {
      doEndElement(diSimple, contentHandler)
      true
    } catch {
      case _: SAXException => false
    }
  }

  override def startComplex(diComplex: DIComplex): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    try {
      doStartElement(diComplex, contentHandler)
      true
    } catch {
      case _: SAXException => false
    }
  }

  override def endComplex(diComplex: DIComplex): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    try {
      doEndElement(diComplex, contentHandler)
      true
    } catch {
      case _: SAXException => false
    }
  }

  override def startArray(diArray: DIArray): Boolean = true // not applicable

  override def endArray(diArray: DIArray): Boolean = true // not applicable

  private def createNilAttribute(): AttributesImpl = {
    val attrs = new AttributesImpl()
    attrs.addAttribute(XMLUtils.XSI_NAMESPACE, "nil", "xsi:nil", "", "true")
    attrs
  }

  private def doStartPrefixMapping(diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val nsbStart = diElem.erd.minimizedScope
    val nsbEnd = if (diElem.isRoot) {
      scala.xml.TopScope
    } else {
      diElem.diParent.erd.minimizedScope
    }
    var n = nsbStart
    while (n != nsbEnd) {
      val prefix = if (n.prefix == null) "" else n.prefix
      val uri = if (n.uri == null) "" else n.uri
      contentHandler.startPrefixMapping(prefix, uri)
      n = n.parent
    }
  }

  private def doEndPrefixMapping(diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val nsbStart = diElem.erd.minimizedScope
    val nsbEnd = if (diElem.isRoot) {
      scala.xml.TopScope
    } else {
      diElem.diParent.erd
        .minimizedScope
    }
    var n = nsbStart
    while (n != nsbEnd) {
      val prefix = if (n.prefix == null) "" else n.prefix
      contentHandler.endPrefixMapping(prefix)
      n = n.parent
    }
  }

  private def doStartElement(diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val (ns: String, elemName: String, qName: String) = getNameSpaceElemNameAndQName(diElem)
    doStartPrefixMapping(diElem, contentHandler)

    val attrs = if (isNilled(diElem)) {
      createNilAttribute()
    } else {
      new AttributesImpl()
    }

    contentHandler.startElement(ns, elemName, qName, attrs)
  }

  private def doEndElement (diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val (ns: String, elemName: String, qName: String) = getNameSpaceElemNameAndQName(diElem)
    contentHandler.endElement(ns, elemName, qName)
    doEndPrefixMapping(diElem, contentHandler)
  }

  private def getNameSpaceElemNameAndQName(
    diElem: DIElement): (String, String, String) = {
    val ns: String =
      if (diElem.erd.namedQName.namespace.isNoNamespace) {
        ""
      } else {
        diElem.erd.namedQName.namespace.toString
      }
    val elemName = diElem.erd.namedQName.local
    val qName = diElem.erd.namedQName.toQNameString
    (ns, elemName, qName)
  }

}

class DaffodilOutputContentHandler(out: OutputStream, pretty: Boolean = false)
  extends ContentHandler with Indentable with XMLInfosetOutputter {
  private val writer = new OutputStreamWriter(out)
  private var prefixMapping: NamespaceBinding = null
  private val outputNewlineStack: MStackOfBoolean = {
    val s = MStackOfBoolean()
    s.push(false)
    s
  }

  // if the top of the stack is true, we have guessed we should output a newline
  private def outputNewline: Boolean = outputNewlineStack.top

  def reset(): Unit = {
    resetIndentation()
    writer.flush()
    prefixMapping = null
    outputNewlineStack.clear()
    outputNewlineStack.push(false) //to match initialization state
    out.flush()
  }

  override def setDocumentLocator(locator: Locator): Unit = {
    // do nothing
  }

  override def startDocument(): Unit = {
    writer.write("""<?xml version="1.0" encoding="UTF-8"?>""")
  }

  override def endDocument(): Unit = {
    writer.write(System.lineSeparator())
    writer.flush()
  }

  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    val _prefix = if (prefix == "") null else prefix
    prefixMapping = NamespaceBinding(_prefix, uri, prefixMapping)
  }

  override def endPrefixMapping(prefix: String): Unit = {
  // do nothing
  }

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    // the pop/true removes whatever is on the stack which is our previous guess for whether we
    // would need a newline after the previous end tag. As we are currently at the start of a new
    // tag, we want to correct that assumption (in case it was false)
    outputNewlineStack.pop()
    outputNewlineStack.push(true)
    if (pretty) {
      writer.write(System.lineSeparator())
      outputIndentation(writer)
    }
    // handle start of tag
    writer.write("<")
    writer.write(qName)
    // handle attributes
    for (i <- 0 until atts.getLength) {
      val attsValue = atts.getValue(i)
      val attsQName = atts.getQName(i)
      writer.write(s""" $attsQName="$attsValue"""")
    }
    // handle namespaces
    if (prefixMapping != null) {
      val pm = prefixMapping.toString()
      writer.write(pm)
      prefixMapping = null
    }
    // handle end of tag
    writer.write(">")
    incrementIndentation()
    // this push makes the assumption that we would not need to output a newline after this end
    // tag is complete
    outputNewlineStack.push(false)
  }

  override def endElement(uri: String, localName: String,  qName: String): Unit = {
    decrementIndentation()
    if (outputNewline) {
      if (pretty) {
        writer.write(System.lineSeparator())
        outputIndentation(writer)
      }
    }
    writer.write("</")
    writer.write(qName)
    writer.write(">")
    outputNewlineStack.pop()
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    writer.write(ch, start, length)
  }

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    // do nothing
  }

  override def processingInstruction(target: String, data: String): Unit = {
    // do nothing
  }

  override def skippedEntity(name: String): Unit = {
    // do nothing
  }
}
