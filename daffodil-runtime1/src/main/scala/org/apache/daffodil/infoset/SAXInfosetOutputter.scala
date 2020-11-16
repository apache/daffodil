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

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.xml.XMLUtils
import org.xml.sax.ContentHandler
import org.xml.sax.SAXException
import org.xml.sax.helpers.AttributesImpl

class SAXInfosetOutputter(xmlReader: DFDL.DaffodilParseXMLReader)
  extends InfosetOutputter
  with XMLInfosetOutputter {
  /**
   * Reset the internal state of this InfosetOutputter. This should be called
   * inbetween calls to the parse method.
   */
  override def reset(): Unit = {
    // this doesn't do anything as the ContentHandler API does not support
    // resetting, but some implemented ContentHandlers, such as the JDOM SAXHandler,
    // do support resetting so it's up to the creator of the contentHandler, to call
    // their contentHandler's reset if applicable and if necessary
  }

  override def startDocument(): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      try {
        contentHandler.startDocument()
        true
      } catch {
        case _: SAXException => false
      }
    } else {
      true
    }
  }

  override def endDocument(): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      try {
        contentHandler.endDocument()
        true
      } catch {
        case _: SAXException => false
      }
    } else {
      true
    }
  }

  override def startSimple(diSimple: DISimple): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
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
    } else {
      true
    }
  }

  override def endSimple(diSimple: DISimple): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      try {
        doEndElement(diSimple, contentHandler)
        true
      } catch {
        case _: SAXException => false
      }
    } else {
      true
    }
  }

  override def startComplex(diComplex: DIComplex): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      try {
        doStartElement(diComplex, contentHandler)
        true
      } catch {
        case _: SAXException => false
      }
    } else {
      true
    }
  }

  override def endComplex(diComplex: DIComplex): Boolean = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      try {
        doEndElement(diComplex, contentHandler)
        true
      } catch {
        case _: SAXException => false
      }
    } else {
      true
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
    while (n != nsbEnd && n != null && n != scala.xml.TopScope) {
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
    while (n != nsbEnd && n != null && n != scala.xml.TopScope) {
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


