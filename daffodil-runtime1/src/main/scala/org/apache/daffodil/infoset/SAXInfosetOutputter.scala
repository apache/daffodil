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

import scala.xml.NamespaceBinding

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.xml.XMLUtils
import org.xml.sax.ContentHandler
import org.xml.sax.SAXException
import org.xml.sax.helpers.AttributesImpl

class SAXInfosetOutputter(xmlReader: DFDL.DaffodilParseXMLReader,
  val namespacesFeature: Boolean,
  val namespacePrefixesFeature: Boolean)
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

  private def doStartPrefixMapping(diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val (nsbStart: NamespaceBinding, nsbEnd: NamespaceBinding) = getNsbStartAndEnd(diElem)
    var n = nsbStart
    while (n.ne(nsbEnd) && n.ne(null) && n.ne(scala.xml.TopScope)) {
      val prefix = if (n.prefix == null) "" else n.prefix
      val uri = if (n.uri == null) "" else n.uri
      contentHandler.startPrefixMapping(prefix, uri)
      n = n.parent
    }
  }

  private def doEndPrefixMapping(diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val (nsbStart: NamespaceBinding, nsbEnd: NamespaceBinding) = getNsbStartAndEnd(diElem)
    var n = nsbStart
    while (n.ne(nsbEnd) && n.ne(null) && n.ne(scala.xml.TopScope)) {
      val prefix = if (n.prefix == null) "" else n.prefix
      contentHandler.endPrefixMapping(prefix)
      n = n.parent
    }
  }

  /**
   * Add the prefixes and uris from the element's NamespaceBinding to Attributes,
   * when namespacePrefixes feature is true
   */
  private def doAttributesPrefixMapping(diElem: DIElement, attrs: AttributesImpl): AttributesImpl = {
    val (nsbStart: NamespaceBinding, nsbEnd: NamespaceBinding) = getNsbStartAndEnd(diElem)
    var n = nsbStart
    while (n.ne(nsbEnd) && n.ne(null) && n.ne(scala.xml.TopScope)) {
      val prefix = if (n.prefix == null) "xmlns" else s"xmlns:${n.prefix}"
      val uri = if (n.uri == null) "" else n.uri
      // uri and localname are always empty for NamespaceBinding attributes
      attrs.addAttribute("", "", prefix, "CDATA", uri)
      n = n.parent
    }
    attrs
  }

  private def getNsbStartAndEnd(diElem: DIElement) = {
    val nsbStart = diElem.erd.minimizedScope
    val nsbEnd = if (diElem.isRoot) {
      scala.xml.TopScope
    } else {
      diElem.diParent.erd.minimizedScope
    }
    (nsbStart, nsbEnd)
  }

  private def doStartElement(diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val (ns: String, localName: String, qName: String) = getNamespaceLocalNameAndQName(diElem)
    val attrs = new AttributesImpl()
    val elemUri: String = if (namespacesFeature) ns else ""
    val elemLocalName: String = if (namespacesFeature) localName else ""
    val elemQname: String = if (namespacePrefixesFeature) qName else ""

    if (namespacesFeature) {
      // only when this feature is true do we use prefix mappings
      doStartPrefixMapping(diElem, contentHandler)
    }

    if (namespacePrefixesFeature) {
      // handle prefix attribute
      doAttributesPrefixMapping(diElem, attrs)
    }

    // handle xsi:nil attribute
    if (diElem.isNilled) {
      val isNilled = "true"
      val nType: String = "CDATA"
      val nValue: String = isNilled
      val nQname = if (namespacePrefixesFeature) "xsi:nil" else ""
      val nUri: String = if (namespacesFeature) XMLUtils.XSI_NAMESPACE else ""
      val nLocalName: String = if (namespacesFeature) "nil" else ""

      attrs.addAttribute(nUri, nLocalName, nQname, nType, nValue)
    }

    contentHandler.startElement(elemUri, elemLocalName, elemQname, attrs)
  }

  private def doEndElement (diElem: DIElement, contentHandler: ContentHandler): Unit = {
    val (ns: String, localName: String, qName: String) = getNamespaceLocalNameAndQName(diElem)
    val elemUri: String = if (namespacesFeature) ns else ""
    val elemLocalName = if (namespacesFeature) localName else ""
    val elemQname = if (namespacePrefixesFeature) qName else ""

    contentHandler.endElement(elemUri, elemLocalName, elemQname)

    // only when this feature is true do we use prefix mappings
    if (namespacesFeature) doEndPrefixMapping(diElem, contentHandler)
  }

  private def getNamespaceLocalNameAndQName(
    diElem: DIElement): (String, String, String) = {
    val ns: String =
      if (diElem.erd.namedQName.namespace.isNoNamespace) {
        ""
      } else {
        diElem.erd.namedQName.namespace.toString
      }
    val elemName = diElem.erd.namedQName.local
    val qName = diElem.erd.prefixedName
    (ns, elemName, qName)
  }

}


