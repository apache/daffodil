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

import java.io.StringWriter
import java.lang.{ Boolean => JBoolean }
import java.nio.charset.StandardCharsets
import javax.xml.XMLConstants
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants._
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamWriter
import javax.xml.stream.util.XMLEventAllocator
import scala.jdk.CollectionConverters._

import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType
import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType._
import org.apache.daffodil.api.infoset.{ InfosetInputter => JInfosetInputter }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo

object XMLTextInfoset {
  lazy val xmlInputFactory = {
    val fact = new com.ctc.wstx.stax.WstxInputFactory()
    // Disable coalescing. We use getElementText() almost everywhere in this
    // inputter, which coalesces simple type content regardless of this
    // property. This is what we want in most cases since we need to coalesce
    // simple type content into a single string for Daffodil, so we might as
    // well let the XMLStreamReader do it. The one exception to this is in our
    // gatherXmlAsString() function. In this function, we want XML events to
    // match the actual XML as closely as possible to avoid making changes
    // where possible. We can achieve this by disabling coalescing, and not use
    // getElementText() when needed.
    fact.setProperty(XMLInputFactory.IS_COALESCING, false)
    fact.setEventAllocator(com.ctc.wstx.evt.DefaultEventAllocator.getDefaultInstance)

    // Woodstox has it's own properties for turning off DTDs to provide
    // secure XML processing.
    //
    // This does not cause Woodstox to throw or error on encountering
    // a DTD. It just doesn't do anything with it.
    //
    fact.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    fact.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    fact.setProperty(XMLInputFactory.IS_VALIDATING, false) // no DTD validation
    fact
  }

  lazy val xmlOutputFactory = {
    val fact = new com.ctc.wstx.stax.WstxOutputFactory()
    fact
  }

  /**
   * When the stringAsXml runtime property is set to true we treat the simple
   * content as if it were XML and output it to the infoset. We use this value
   * as the runtime property name. Additionally, when we output the XML we need
   * a wrapper element to reset the default XML namespace in case the infoset
   * has a default namespace. We also use this variable as the name of that
   * wrapper element.
   */
  lazy val stringAsXml = "stringAsXml"

  /**
   * Read an event from an XMLStreamReader and write the same event to an
   * XMLStreamWriter. For example, this can be used for convert XML from one
   * type of input to a different type (e.g. InputStream to String), check for
   * well-formed XML, and perform some basic normalization. Example usage of
   * this function is to call this in a loop like this:
   *
   *   while (xsr.hasNext()) {
   *     xsr.next()
   *     XMLTextInfoset.writeStreamEvent(xsr, xsw)
   *   }
   *
   * This approach allows callers to skip certain events by conditionally
   * calling this function.
   *
   * It is assumed that the caller calls xsw.startDocument() if needed. This
   * function does not handle the START_DOCUMENT event.
   *
   * Because we disable coalescing in our XMLStreamReader, we will actually get
   * events for CHARACTERS, CDATA, SPACE, and ENTITY_REFERENCE, which would
   * otherwise be lost. This allows us to convert XML to strings (e.g. in
   * gatherXmlAsString) that are much closer to the original XML, and in some
   * cases exactly the same.
   *
   * This function drops all DTD events. This is because this function is used
   * to either embed XML into the middle of an XML infoset, or read an embedded
   * XML from the middle of an XML infoset. In either case DTD's aren't allowed
   * because they are only allowed at the top of XML, and so we drop them.
   *
   * Spaces inside element tags (e.g. in between attributes/namespce
   * declarations) are not preserved.
   *
   * Attribute and namespace definitions order is preserved, but all namespace
   * definitions are output first followed by all attributes.
   *
   * Element/attribute prefixes are preserved
   *
   * Comments are preserved
   *
   * CDATA regions are preserved
   *
   * Processing instructions are preserved.
   *
   * The XML declaration is always output when unparsing, and never output when
   * parsing (handled by the callers).
   *
   * Double quotes are always used for attribute/namespace definitions, except
   * in the XML declaration which always uses single quotes.
   *
   * The following character entities are preserved: &lt; &amp; &#xd;. The
   * decimal entitity &#10; is converted to the &#xd; hex entity. All others
   * are converted to thir UTF-8 characters. Because DTD is disabled, custom
   * entities are not supported and result in an error.
   *
   * Whitespace, both inbetween complex elements and inside simple elements, are
   * preserved. However, whitespace in the epilog or prolog is ignored.
   *
   * Elements with no content but with an open and close tag are converted to a
   * an empty-element tag.
   *
   * Both a lone CR and CRLF are converted to LF.
   */
  def writeXMLStreamEvent(xsr: XMLStreamReader, xsw: XMLStreamWriter): Unit = {
    xsr.getEventType() match {
      case START_ELEMENT => {
        xsw.writeStartElement(xsr.getPrefix(), xsr.getLocalName(), xsr.getNamespaceURI())
        for (i <- 0 until xsr.getNamespaceCount()) {
          xsw.writeNamespace(xsr.getNamespacePrefix(i), xsr.getNamespaceURI(i))
        }
        for (i <- 0 until xsr.getAttributeCount()) {
          xsw.writeAttribute(
            xsr.getAttributePrefix(i),
            xsr.getAttributeNamespace(i),
            xsr.getAttributeLocalName(i),
            xsr.getAttributeValue(i)
          )
        }
      }
      case END_ELEMENT => xsw.writeEndElement()
      case CHARACTERS => xsw.writeCharacters(xsr.getText())
      case COMMENT => xsw.writeComment(xsr.getText())
      case CDATA => xsw.writeCData(xsr.getText())
      case PROCESSING_INSTRUCTION =>
        xsw.writeProcessingInstruction(xsr.getPITarget(), xsr.getPIData())
      case END_DOCUMENT => xsw.writeEndDocument()
      case DTD => {
        // even though we disable DTD in the XMLInputFactory, we still get DTD
        // events when parsing XML. Silently ignore these events
      }
      // $COVERAGE-OFF$
      case START_DOCUMENT | ATTRIBUTE | NAMESPACE | ENTITY_REFERENCE | SPACE | _ => {
        // START_DOCUMENT events are expected to be handled and skipped by
        // the caller.
        //
        // Woodstox does not create ATTRIBUTE or NAMESPACE events. Namespace
        // prefix definition and attributes are accessable only from the
        // START_ELEMENT.
        //
        // Woodstox does not create SPACE events.
        //
        // ENTITY_REFERENCE events are never created. This event is only used
        // for custom entities, which error because we disable DTD.
        Assert.invariantFailed("Unexpected XML event while XML stream: " + xsr.getEventType())
      }
      // $COVERAGE-ON$
    }
  }
}

class XMLTextInfosetInputter(input: java.io.InputStream) extends JInfosetInputter {

  /**
   * evAlloc is only to be used for diagnostic messages. It lets us easily
   * capture and toString the event information. But we don't call it otherwise
   * as it allocates an object, and we're trying to avoid that.
   */
  private lazy val (xsr: XMLStreamReader, evAlloc: XMLEventAllocator) = {
    val xsr = XMLTextInfoset.xmlInputFactory.createXMLStreamReader(input)

    //
    // This gets the event allocator corresponding to the xmlStreamReader just created.
    // Strange API. They should let you get this from the xmlStreamReader itself.
    //
    val evAlloc = XMLTextInfoset.xmlInputFactory.getEventAllocator

    // no need for UnparseError here. If the XML syntax is bad, parser catches it before we get here.
    Assert.invariant(xsr.hasNext())
    val evNum = xsr.getEventType()
    Assert.invariant(evNum == START_DOCUMENT)
    (xsr, evAlloc)
  }

  /**
   * Used to force a StartElement event to occur even when xsr.getEventType
   * would tell us to do an EndElement event. This is because getSimpleText
   * moves the xsr to the end event without next() ever being called. So if
   * getSimpleText is called, we set this to true to keep the current
   * StartEvent until next() is called, at which point it is set back to false
   * so the EndElement even is produced.
   */
  private var fakeStartEvent = false

  override def getEventType(): InfosetInputterEventType = {
    if (fakeStartEvent) {
      StartElement
    } else {
      xsr.getEventType() match {
        case START_DOCUMENT => StartDocument
        case END_DOCUMENT => EndDocument
        case START_ELEMENT => StartElement
        case END_ELEMENT => EndElement
      }
    }
  }

  override def getLocalName(): String = {
    xsr.getLocalName()
  }

  override def getSupportsNamespaces = true

  override def getNamespaceURI(): String = {
    xsr.getNamespaceURI()
  }

  /**
  * Consumes all events inside a "stringAsXml" element and forwards those
  * events to an XMLStreamWriter to write them to a string. That string is
  * returned and used as the value of the simple element.
   */
  private def gatherXmlAsString(): String = {
    // move past the START_ELEMENT event for this simple type, ignoring all
    // whitespace/comments
    xsr.nextTag()

    // we should now be at the START_ELEMENT event for the wrapper element.
    // We need to skip it. Error if that's not the case.
    if (
      xsr.getEventType() != START_ELEMENT || xsr.getLocalName() != XMLTextInfoset.stringAsXml
    ) {
      throw new XMLStreamException("Expected start of " + XMLTextInfoset.stringAsXml)
    }
    xsr.next()

    // we are now at the first event inside the wrapper element. Convert this
    // and all following events we see to a string until we find the closing
    // wrapper tag. We trim the result to remove whitespace that the outputter
    // may have written with pretty mode enabled.
    val sw = new StringWriter()
    val xsw =
      XMLTextInfoset.xmlOutputFactory.createXMLStreamWriter(sw, StandardCharsets.UTF_8.toString)
    xsw.writeStartDocument()
    while (
      xsr.getEventType() != END_ELEMENT || xsr.getLocalName() != XMLTextInfoset.stringAsXml
    ) {
      XMLTextInfoset.writeXMLStreamEvent(xsr, xsw)
      xsr.next()
    }
    xsw.writeEndDocument()
    val xmlString = sw.toString.trim

    // skip the END_ELEMENT event for the wrapper element and any following whitespace
    xsr.nextTag()

    // should now be at the END_ELEMENT for our simple type
    if (xsr.getEventType() != END_ELEMENT) {
      throw new XMLStreamException(
        "Expected end of element following end of " + XMLTextInfoset.stringAsXml
      )
    }

    xmlString
  }

  override def getSimpleText(primType: NodeInfo.Kind): String =
    getSimpleText(primType, Map[String, String](XMLTextInfoset.stringAsXml -> "false").asJava)

  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String = {

    val txt =
      if (
        primType == NodeInfo.String && runtimeProperties.get(
          XMLTextInfoset.stringAsXml
        ) == "true"
      ) {
        try {
          gatherXmlAsString()
        } catch {
          case xse: XMLStreamException => {
            val lineNum = evAlloc.allocate(xsr).getLocation.getLineNumber
            throw new InvalidInfosetException(
              "Error on line " + lineNum + ": " + xse.getMessage
            )
          }
        }
      } else {
        val elementText =
          try {
            xsr.getElementText()
          } catch {
            case xse: XMLStreamException => {
              throw new NonTextFoundInSimpleContentException(
                "Error on line " + evAlloc.allocate(xsr).getLocation.getLineNumber
              )
            }
          }
        if (primType == NodeInfo.String) {
          XMLUtils.remapPUAToXMLIllegalCharacters(elementText)
        } else {
          elementText
        }
      }

    // getElementText and gatherXmlAsString move the current event to the
    // EndElement. We want to stay on StartElement until next is called. So set
    // fakeStartEvent to true so that any calls to getEventType will return
    // StartElement.
    Assert.invariant(xsr.getEventType() == END_ELEMENT)
    fakeStartEvent = true

    txt
  }

  override def isNilled(): Option[JBoolean] = {
    Assert.invariant(xsr.getEventType() == START_ELEMENT)
    // this should use a fast hash lookup
    val nilAttrValue = xsr.getAttributeValue(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil")
    val res: Option[JBoolean] =
      if (nilAttrValue == null) {
        None
      } else if (nilAttrValue == "true" || nilAttrValue == "1") {
        Some(true)
      } else if (nilAttrValue == "false" || nilAttrValue == "0") {
        Some(false)
      } else {
        throw new InvalidInfosetException(
          "xsi:nil property is not a valid boolean: '" + nilAttrValue + "' on line " + evAlloc
            .allocate(xsr)
            .getLocation
            .getLineNumber
        )
      }
    res
  }

  override def fini(): Unit = {
    xsr.close()
  }

  override def hasNext(): Boolean = {
    xsr.hasNext()
  }

  override def next(): Unit = {
    if (fakeStartEvent) {
      // we are faking a StartElement event due to a call to getSimpleText or
      // gatherXmlAsString. Now that we have called next() we need to return an
      // EndElement event. The xsr is already on the end event for this
      // element, so all we need to to is flip the fakeStartEvent flag and
      // we'll get the correct EndElement event
      fakeStartEvent = false
    } else {
      val next = nextTagOrEndDocument()
      if (next == -1) {
        // should not have been called, we finished
        Assert.abort()
      }
    }
  }

  /**
   * Almost same as xsr.nextTag(), but returns normally on
   * END_DOCUMENT as well as START_ELEMENT and END_ELEMENT
   *
   * returns -1 if called after END_DOCUMENT.
   */
  private def nextTagOrEndDocument(): Int = {
    var result: Int = -1
    var exitNow = false
    while (xsr.hasNext() && !exitNow) {
      result =
        try {
          xsr.next()
        } catch {
          case xse: XMLStreamException => {
            val details = "Error: " + Misc
              .getSomeMessage(xse)
              .get + " on line " + evAlloc.allocate(xsr).getLocation.getLineNumber
            throw new IllegalContentWhereEventExpected(details)
          }
        }
      result match {
        case START_ELEMENT | END_ELEMENT | END_DOCUMENT => exitNow = true
        case CHARACTERS if xsr.isWhiteSpace() => // skip whitespace
        case CDATA if xsr.isWhiteSpace() => // skip whitespace
        case SPACE | PROCESSING_INSTRUCTION | COMMENT => // skip these too
        case DTD =>
          throw new IllegalContentWhereEventExpected(
            "DOCTYPE/DTD Not supported. Error on line " + evAlloc
              .allocate(xsr)
              .getLocation
              .getLineNumber
          )
        case other =>
          throw new IllegalContentWhereEventExpected(
            "Error on line " + evAlloc.allocate(xsr).getLocation.getLineNumber + " : " + other
          )
      }
    }
    result
  }
}
