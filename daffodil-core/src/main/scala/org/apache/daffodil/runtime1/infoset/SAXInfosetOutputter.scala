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

import scala.xml.NamespaceBinding

import org.apache.daffodil.api.DFDLPrimType
import org.apache.daffodil.api.InfosetArray
import org.apache.daffodil.api.InfosetComplexElement
import org.apache.daffodil.api.InfosetElement
import org.apache.daffodil.api.InfosetSimpleElement
import org.apache.daffodil.api.infoset.{ InfosetOutputter => JInfosetOutputter }
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.iapi.DFDL

import org.xml.sax.ContentHandler
import org.xml.sax.helpers.AttributesImpl

class SAXInfosetOutputter(
  xmlReader: DFDL.DaffodilParseXMLReader,
  val namespacesFeature: Boolean,
  val namespacePrefixesFeature: Boolean
) extends JInfosetOutputter {

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

  override def startDocument(): Unit = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      contentHandler.startDocument()
    }
  }

  override def endDocument(): Unit = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      contentHandler.endDocument()
    }
  }

  override def startSimple(simple: InfosetSimpleElement): Unit = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      doStartElement(simple, contentHandler)
      if (!simple.isNilled) {
        val text =
          if (simple.metadata.dfdlType == DFDLPrimType.String) {
            XMLUtils.remapXMLIllegalCharactersToPUA(simple.getText)
          } else {
            simple.getText
          }
        val arr = text.toCharArray
        contentHandler.characters(arr, 0, arr.length)
      }
    }
  }

  override def endSimple(simple: InfosetSimpleElement): Unit = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      doEndElement(simple, contentHandler)
    }
  }

  override def startComplex(complex: InfosetComplexElement): Unit = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      doStartElement(complex, contentHandler)
    }
  }

  override def endComplex(complex: InfosetComplexElement): Unit = {
    val contentHandler = xmlReader.getContentHandler
    if (contentHandler != null) {
      doEndElement(complex, contentHandler)
    }
  }

  override def startArray(ar: InfosetArray): Unit = {} // not applicable

  override def endArray(ar: InfosetArray): Unit = {} // not applicable

  private def doStartPrefixMapping(
    elem: InfosetElement,
    contentHandler: ContentHandler
  ): Unit = {
    val (nsbStart: NamespaceBinding, nsbEnd: NamespaceBinding) = getNsbStartAndEnd(elem)
    var n = nsbStart
    while (n.ne(nsbEnd) && n.ne(null) && n.ne(scala.xml.TopScope)) {
      val prefix = if (n.prefix == null) "" else n.prefix
      val uri = if (n.uri == null) "" else n.uri
      contentHandler.startPrefixMapping(prefix, uri)
      n = n.parent
    }
  }

  private def doEndPrefixMapping(elem: InfosetElement, contentHandler: ContentHandler): Unit = {
    val (nsbStart: NamespaceBinding, nsbEnd: NamespaceBinding) = getNsbStartAndEnd(elem)
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
  private def doAttributesPrefixMapping(
    elem: InfosetElement,
    attrs: AttributesImpl
  ): AttributesImpl = {
    val (nsbStart: NamespaceBinding, nsbEnd: NamespaceBinding) = getNsbStartAndEnd(elem)
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

  /**
   * Copied (with slight modification) from Scala-XML NamespaceBinding.scala to
   * ensure we use the same logic to convert NamespaceBindings to mappings as
   * other InfosetOutputters that use NamespaceBinding.buildString(stop)
   */
  private def shadowRedefined(
    start: NamespaceBinding,
    stop: NamespaceBinding
  ): NamespaceBinding = {
    def prefixList(x: NamespaceBinding): List[String] =
      if ((x == null) || (x eq stop)) Nil
      else x.prefix :: prefixList(x.parent)

    // $COVERAGE-OFF$ See below comment for why coverage is disabled
    def fromPrefixList(l: List[String]): NamespaceBinding = l match {
      case Nil => stop
      case x :: xs => new NamespaceBinding(x, start.getURI(x), fromPrefixList(xs))
    }
    // $COVERAGE-ON$

    val ps0 = prefixList(start).reverse
    val ps = ps0.distinct
    if (ps.size == ps0.size) start
    else {
      // $COVERAGE-OFF$
      // this branch is only hit when the size of ps0 and ps0.distinct are
      // different, i.e. there are duplicate prefix mappings in the namespace
      // binding. But these namespace bindings come from minimizedScope, which
      // removes duplicate prefixes. So it should be impossible for this branch
      // to get hit--all mappings should already be distinct. So this branch
      // and the fromPrefixList function should both be dead code. To make it
      // more clear that this is copied from Scala XML, and in case there is an
      // edge case bug related to minimizedScope that we don't have a test for,
      // it is safest to just keep this logic from ScalaXML and just disable
      // code coverage.
      fromPrefixList(ps)
      // $COVERAGE-ON$
    }
  }

  private def getNsbStartAndEnd(elem: InfosetElement): (NamespaceBinding, NamespaceBinding) = {
    val diElem = elem.asInstanceOf[DIElement]
    val nsbStart = diElem.erd.minimizedScope
    val nsbEnd = if (diElem.isRoot) {
      scala.xml.TopScope
    } else {
      diElem.diParent.erd.minimizedScope
    }

    // the callers of getNsbStartAndEnd use reference equality to determine
    // what mappings to create. But minimizedScope is broken (DAFFODIL-2282),
    // so sometimes reference equality can sometimes result in incorrect
    // mappings. So we first check if the two NamespaceBindings have value
    // equality--if they have the same value then no namespace mappings are
    // needed, and we just return (start, start). If they aren't value equal,
    // then we need to shadow redefine the start in terms of the end, and use
    // that as the new start (this is same logic as used by NamespaceBinding.buildString
    // in other infoset outputters). This is all a hack to get consistent
    // namespace bindings among SAX and different infoset outputters without
    // needing to fix minimizedScope. Once that is fixed, this should be able
    // to be removed.
    if (nsbStart == nsbEnd) {
      (nsbStart, nsbStart)
    } else {
      (shadowRedefined(nsbStart, nsbEnd), nsbEnd)
    }
  }

  private def doStartElement(elem: InfosetElement, contentHandler: ContentHandler): Unit = {
    val (ns: String, localName: String, qName: String) = getNamespaceLocalNameAndQName(elem)
    val attrs = new AttributesImpl()
    val elemUri: String = if (namespacesFeature) ns else ""
    val elemLocalName: String = if (namespacesFeature) localName else ""
    val elemQname: String = if (namespacePrefixesFeature) qName else ""

    if (namespacesFeature) {
      // only when this feature is true do we use prefix mappings
      doStartPrefixMapping(elem, contentHandler)
    }

    if (namespacePrefixesFeature) {
      // handle prefix attribute
      doAttributesPrefixMapping(elem, attrs)
    }

    // handle xsi:nil attribute
    if (elem.isNilled) {
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

  private def doEndElement(elem: InfosetElement, contentHandler: ContentHandler): Unit = {
    val (ns: String, localName: String, qName: String) = getNamespaceLocalNameAndQName(elem)
    val elemUri: String = if (namespacesFeature) ns else ""
    val elemLocalName = if (namespacesFeature) localName else ""
    val elemQname = if (namespacePrefixesFeature) qName else ""

    contentHandler.endElement(elemUri, elemLocalName, elemQname)

    // only when this feature is true do we use prefix mappings
    if (namespacesFeature) doEndPrefixMapping(elem, contentHandler)
  }

  private def getNamespaceLocalNameAndQName(elem: InfosetElement): (String, String, String) = {
    val ns: String =
      if (elem.metadata.namespace eq null) {
        ""
      } else {
        elem.metadata.namespace
      }
    val elemName = elem.metadata.name
    val qName = elem.asInstanceOf[DIElement].erd.prefixedName
    (ns, elemName, qName)
  }
}
