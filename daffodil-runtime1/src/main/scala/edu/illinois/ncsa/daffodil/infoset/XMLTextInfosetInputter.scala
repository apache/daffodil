/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import edu.illinois.ncsa.daffodil.dpath.NodeInfo

import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamConstants._
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.util.XMLEventAllocator
import javax.xml.stream.XMLStreamException
import javax.xml.XMLConstants

object XMLTextInfosetInputter {
  lazy val xmlInputFactory = {
    val fact = new com.ctc.wstx.stax.WstxInputFactory()
    // JIRA DFDL-1659 - make sure not accessing things remotely and protect from denial-of-service
    // using XML trickery.
    // fact.setProperty("http://javax.xml.XMLConstants/property/accessExternalDTD", false)
    // fact.setProperty("http://xml.org/sax/features/external-general-entities", false)
    // fact.setProperty("http://xml.org/sax/features/external-parameter-entities", false)
    //
    // fact.setProperty(XMLConstants.FEATURE_SECURE_PROCESSING, true) // Seems to be the default setting anyway

    fact.setProperty(XMLInputFactory.IS_COALESCING, true)
    fact.setEventAllocator(com.ctc.wstx.evt.DefaultEventAllocator.getDefaultInstance)

    fact
  }
}

class XMLTextInfosetInputter(reader: java.io.Reader)
  extends InfosetInputter {

  /**
   * evAlloc is only to be used for diagnostic messages. It lets us easily
   * capture and toString the event information. But we don't call it otherwise
   * as it allocates an object, and we're trying to avoid that.
   */
  private lazy val (xsr: XMLStreamReader, evAlloc: XMLEventAllocator) = {
    val xsr = XMLTextInfosetInputter.xmlInputFactory.createXMLStreamReader(reader)
    //
    // This gets the event allocator corresponding to the xmlStreamReader just created.
    // Strange API. They should let you get this from the xmlStreamReader itself.
    //
    val evAlloc = XMLTextInfosetInputter.xmlInputFactory.getEventAllocator

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

  override def getLocalName: String = {
    xsr.getLocalName()  
  }

  override val supportsNamespaces = true

  override def getNamespaceURI(): String = {
    xsr.getNamespaceURI()
  }

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    val txt =
      try {
        xsr.getElementText()
      } catch {
        case xse: XMLStreamException => {
          throw new NonTextFoundInSimpleContentException("Error on line " + evAlloc.allocate(xsr).getLocation.getLineNumber)
        }
      }
    Assert.invariant(xsr.getEventType() == END_ELEMENT)
    // getElementText moves the current event to the EndElement. We want to
    // stay on StartElement until next is called. So set fakeStartEvent to true
    // so that any calls to getEventType will return StartElement.
    fakeStartEvent = true
    if (primType.isInstanceOf[NodeInfo.String.Kind]) {
      XMLUtils.remapPUAToXMLIllegalCharacters(txt)
    } else {
      txt
    }
  }

  override def isNilled(): MaybeBoolean = {
    Assert.invariant(xsr.getEventType() == START_ELEMENT)
    // this should use a fast hash lookup
    val nilAttrValue = xsr.getAttributeValue(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil")
    val res =
      if (nilAttrValue == null) {
        MaybeBoolean.Nope
      } else if (nilAttrValue == "true" || nilAttrValue == "1") {
        MaybeBoolean(true)
      } else if (nilAttrValue == "false" || nilAttrValue == "0") {
        MaybeBoolean(false)
      } else {
        throw new InvalidInfosetException("xsi:nil property is not a valid boolean: '" + nilAttrValue + "' on line " + evAlloc.allocate(xsr).getLocation.getLineNumber)
      }
    res
  }

  override def fini: Unit = {
    xsr.close()
  }

  override def hasNext(): Boolean = {
    xsr.hasNext()
  }

  override def next(): Unit = {
    if (fakeStartEvent) {
      // we are faking a StartElement event due to a call to getSimpleText. Now
      // that we have called next() we need to return an EndElement event. The
      // xsr is already on the end event for this element (due to
      // getSimpleText), so all we need to to is flip the fakeStartEvent
      // variable and we'll get the correct EndElement event
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
            val details = "Error: " + Misc.getSomeMessage(xse).get + " on line " + evAlloc.allocate(xsr).getLocation.getLineNumber
            throw new IllegalContentWhereEventExpected(details)
          }
        }
      result match {
        case START_ELEMENT | END_ELEMENT | END_DOCUMENT => exitNow = true
        case CHARACTERS if xsr.isWhiteSpace() => // skip whitespace
        case CDATA if xsr.isWhiteSpace() => // skip whitespace
        case SPACE | PROCESSING_INSTRUCTION | COMMENT => // skip these too
        case other =>
          throw new IllegalContentWhereEventExpected("Error on line " + evAlloc.allocate(xsr).getLocation.getLineNumber + " : " + other)
      }
    }
    result
  }

}
