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

package org.apache.daffodil.processors

import scala.xml.NamespaceBinding

import javax.xml.XMLConstants
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DFDL.DaffodilUnhandledSAXException
import org.apache.daffodil.api.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.api.DFDL.SAXInfosetEvent
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.IllegalContentWhereEventExpected
import org.apache.daffodil.infoset.InfosetInputterEventType.EndDocument
import org.apache.daffodil.infoset.InfosetInputterEventType.EndElement
import org.apache.daffodil.infoset.InfosetInputterEventType.StartDocument
import org.apache.daffodil.infoset.InfosetInputterEventType.StartElement
import org.apache.daffodil.infoset.SAXInfosetInputter
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.Misc
import org.xml.sax.Attributes
import org.xml.sax.Locator

/**
 * DaffodilUnparseContentHandler produces SAXInfosetEvent objects for the SAXInfosetInputter to
 * consume and convert to events that the Dataprocessor unparse can use. The SAXInfosetEvent object
 * is built from information that is passed to the ContentHandler from an XMLReader parser. In
 * order to receive the uri and prefix information from the XMLReader, the XMLReader must have
 * support for XML Namespaces
 *
 * This class, together with the SAXInfosetInputter, uses coroutines to ensure that a batch of events
 * (based on the tunable saxUnparseEventBatchSize) can be passed from the former to the latter.
 * The following is the general process:
 *
 * - an external call is made to parse an XML Document
 * - this class receives a StartDocument call, which is the first SAXInfosetEvent that should be
 * sent to the SAXInfosetInputter. That event is put onto an array of SAXInfosetEvents of size the
 * saxUnparseEventBatchSize tunable. Once the array is full, it is put on the inputter's queue,
 * this thread is paused, and that inputter's thread is run
 * - when the SAXInfosetInputter is done processing that batch and is ready for a new batch, it
 * sends a 1 element array with the last completed event via the coroutine system, which loads it on
 * the contentHandler's queue, which restarts this thread and pauses that one. In the expected case,
 * the single element array will contain no new information until the unparse complete. In the case of
 * an unexpected error though, it will contain error information
 * - this process continues until the EndDocument SAXInfosetEvent is loaded into the batch.
 * Once that SAXInfosetEvent is processed by the SAXInfosetInputter, it signals the end of batched
 * events coming from the contentHandler. This ends the unparseProcess and returns the event with
 * the unparseResult and/or any error
 * information
 *
 * @param dp dataprocessor object that will be used to call the parse
 * @param output outputChannel of choice where the unparsed data is stored
 */
class DaffodilUnparseContentHandler(
  dp: DFDL.DataProcessor,
  output: DFDL.Output)
  extends DFDL.DaffodilUnparseContentHandler {
  private lazy val inputter = new SAXInfosetInputter(this, dp, output)
  private var unparseResult: DFDL.UnparseResult = _
  private lazy val characterData = new StringBuilder
  private var prefixMapping: NamespaceBinding = _
  private lazy val prefixMappingTrackingStack = new MStackOf[NamespaceBinding]

  private lazy val tunablesBatchSize = dp.tunables.saxUnparseEventBatchSize

  /**
   * we always have an extra buffer in the array that we use for the inputter.hasNext call. For each
   * element, we need to know if it has a viable next, if it doesn't, it will triggers the context
   * switch to DaffodilUnparseContentHandler. So for example, if the user provides 1 as the
   * batchSize, under the hood we'll batch [event1, event2].
   *
   * - DataProcessor.unparse will call hasNext and getEventType for the initialization call
   * - hasNext will check if nextIndex (i.e currentIndex + 1) is non-empty. Since currentIndex is 0,
   * it will return true since event2 exists.
   * - getEventType (which signifies our processing step) is called for the event at currentIndex
   * - After the initialization step, subsequent calls will be a loop of next(), ...some processing
   * of the current event ..., and hasNext()
   * - For our scenario, next() will clear out the contents at currentIndex, increment the currentIndex,
   * and our event2 will be processed, then hasNext will check if there is a viable index 2, as
   * there is not, it will perform the context switch so DaffodilUnparseContentHandler can batch
   * more events
   * - DaffodilUnparseContentHandler copies the last event into the first so the currentEvent stays
   * the same for the inputter until it decides to change it so we end up with [event2, event3]
   * - When we context switch back to inputter.hasNext, it resets the currentIndex to 0, and our loop
   * begins again with a call to next
   *
   * Without us having the extra buffer, things would happen like this:
   * user provides 1 as the batchSize, under the hood we'll have [event1] batched.
   *
   * DataProcessor.unparse will call hasNext and getEventType for the initialization call, and that
   * hasNext will check if cnextIndex (i.e currentIndex + 1) is non-empty. As currentIndex is 0, and
   * it is the maximum index, there is no index 1. It will context switch to get a new batched event,
   * which, would overwrite event1 before we get to process it.
   */
  private lazy val actualBatchSize = tunablesBatchSize + 1
  private lazy val batchedInfosetEvents: Array[SAXInfosetEvent] = {
    Assert.invariant(tunablesBatchSize > 0, "invalid saxUnparseEventBatchSize; minimum value is 1")
    Array.fill[SAXInfosetEvent](actualBatchSize)(new SAXInfosetEvent)
  }
  private var currentIndex: Int = 0

  /**
   * This is a flag that is set to true when startPrefixMapping is called. When true, we make
   * the assumption that we don't need to use the Attributes parameter from startElement to get the
   * namespaceURI information and will solely rely on start/endPrefixMapping. If false, we will use
   * Attributes to get the namespaceURI info.
   */
  private var contentHandlerPrefixMappingUsed = false

  /**
   * returns null in the case of an DaffodilUnhandledSAXException
   */
  def getUnparseResult: DFDL.UnparseResult = unparseResult

  def enableInputterResolutionOfRelativeInfosetBlobURIs(): Unit = inputter.enableResolutionOfRelativeInfosetBlobURIs()

  override def setDocumentLocator(locator: Locator): Unit = {
    // do nothing
  }

  override def startDocument(): Unit = {
    batchedInfosetEvents(currentIndex).eventType = One(StartDocument)
    maybeSendToInputter()
  }

  override def endDocument(): Unit = {
    batchedInfosetEvents(currentIndex).eventType = One(EndDocument)
    maybeSendToInputter()
  }

  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    if (!contentHandlerPrefixMappingUsed) contentHandlerPrefixMappingUsed = true
    val pre = if (prefix == "") null else prefix
    prefixMapping = NamespaceBinding(pre, uri, prefixMapping)
  }

  /**
   * XMLReader does not guarantee the order of the prefixes called for this function, but it does
   * guarantee that this method is called after its corresponding endElement, which means we can
   * can just take off the top mappings, because the element that might have cared about the order
   * is already done using the prefixMappings
   */
  override def endPrefixMapping(prefix: String): Unit = {
    prefixMapping = if (prefixMapping == null) prefixMapping else prefixMapping.parent
  }

  /**
   * Uses Attributes, which is passed in to the startElement callback, to extract prefix mappings and
   * populate the global prefixMapping
   */
  def mapPrefixMappingFromAttributesImpl(atts:Attributes): Unit = {
    var i = 0
    while (i < atts.getLength) {
      val qName = atts.getQName(i)
      val uri =  atts.getValue(i)
      if (qName == "xmlns") {
        prefixMapping = NamespaceBinding(null, uri, prefixMapping)
      } else if (qName.startsWith("xmlns:")) {
        val prefix = qName.substring(6)
        prefixMapping = NamespaceBinding(prefix, uri, prefixMapping)
      } else {
        // do nothing, not a namespace mapping attribute
      }
      i += 1
    }
  }

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    // we need to check if the characters data is all whitespace, if it is we drop the whitespace
    // data, if it is not, it is an error as starting a new element with actual characterData means
    // we haven't hit an endElement yet, which means we're in a complexElement and a complexElement
    // cannot have character content
    if (characterData.nonEmpty && !Misc.isAllWhitespace(characterData)) {
      throw new IllegalContentWhereEventExpected("Non-whitespace characters in complex " +
        "Element: " + characterData.toString
      )
    } else {
      // reset since it was whitespace only
      characterData.setLength(0)
    }

    if (!contentHandlerPrefixMappingUsed) {
      // this is for the situation where the XMLReader doesn't use the start/endPrefixMappings to
      // pass along namespaceMapping informayion, so prefix information must be determined via
      // the Attribute param.
      // we always pushes but mapPrefixMappingFromAttributesImpl won't always add a mapping
      // since atts can be empty
      prefixMappingTrackingStack.push(prefixMapping)
      mapPrefixMappingFromAttributesImpl(atts)
    }

    if (!batchedInfosetEvents(currentIndex).isEmpty && batchedInfosetEvents(currentIndex).localName.isDefined) {
      // we started another element while we were in the process of building a startElement
      // this means the first element was complex and we are ready for the inputter queue
      maybeSendToInputter()
    }
    // use Attributes to determine xsi:nil value
    val nilIn = atts.getIndex(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil")
    batchedInfosetEvents(currentIndex).nilValue = if (nilIn >= 0) {
      val nilValue = atts.getValue(nilIn)
      One(nilValue)
    } else {
      Nope
    }

    // set localName and namespaceURI
    setLocalNameAndNamespaceUri(uri, localName, qName)

    batchedInfosetEvents(currentIndex).eventType = One(StartElement)
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    // if infosetEvent is a startElement, send that first
    if (batchedInfosetEvents(currentIndex).eventType.contains(StartElement)) {
      // any characterData that exists at this point is valid data as padding data has been
      // taken care of in startElement
      val maybeNewStr = One(characterData.toString)
      batchedInfosetEvents(currentIndex).simpleText = maybeNewStr
      characterData.setLength(0)
      maybeSendToInputter()
    }

    batchedInfosetEvents(currentIndex).eventType = One(EndElement)

    // set localName and namespaceURI
    setLocalNameAndNamespaceUri(uri, localName, qName)

    if (!contentHandlerPrefixMappingUsed) {
      // always pops
      prefixMapping = prefixMappingTrackingStack.pop
    }
    maybeSendToInputter()
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    characterData.appendAll(ch, start, length)
  }

  /**
   * we only context swtich to the InfosetInputter if batchedInfosetEvents is full or we hit an
   * EndDocument event
   */
  private def maybeSendToInputter(): Unit = {
    val nextIndex = currentIndex + 1
    if (nextIndex < actualBatchSize &&
      !batchedInfosetEvents(currentIndex).eventType.contains(EndDocument)) {
      // if we have room left on the batchedInfosetEvents array and the current element != EndDocument
      currentIndex += 1
      // at this point where we're loading the contents of the array, it should have been cleared
      // mostly by the InfosetInputter, and the last element by us.
      Assert.invariant(batchedInfosetEvents(currentIndex).isEmpty)
    } else {
      // ready to send it off
      val infosetEventWithResponse = this.resume(inputter, batchedInfosetEvents).head
      // we only ever return a one element array

      // it is possible for unparseResult to be null, in the case of an DaffodilUnhandledSAXException
      if (infosetEventWithResponse.unparseResult.isDefined) {
        unparseResult = infosetEventWithResponse.unparseResult.get
      }
      // any exception is collected and thrown to stop the execution of the xmlReader
      if (infosetEventWithResponse.isError) {
        val causeError = infosetEventWithResponse.causeError.get
        causeError match {
          case unparseError: DaffodilUnparseErrorSAXException =>
            // although this is an expected error, we need to throw it so we can stop the xmlReader
            // parse and this thread
            throw unparseError
          case unhandled: DaffodilUnhandledSAXException => throw unhandled
          case unknown => throw new DaffodilUnhandledSAXException("Unknown exception: ",
            new Exception(unknown))
        }
      }
      // copy the last element into the first for use by inputter becuase that last element was
      // its current element when we did the context switch. When done clear the last element,
      // since the infosetinputter clears all elements except the last one, then set the index to
      // 1 so we can start to load elements starting at the second element
      SAXInfosetEvent.copyEvent(batchedInfosetEvents(currentIndex), batchedInfosetEvents(0))
      batchedInfosetEvents(currentIndex).clear()
      currentIndex = 1
    }
  }

  /**
   * Use the prefixMapping and input parameters to set the localName and NamespaceURI of the
   * SAXInfoset object.
   */
  private def setLocalNameAndNamespaceUri(uri:String, localName: String, qName: String): Unit = {
    lazy val qNameArr = qName.split(":")
    lazy val qNamePrefix = if (qNameArr.length > 1) {//there is a prefix
      qNameArr.head
    } else {
      // if there is no prefix, we attempt to try to get the namespace when the prefix is "". Since
      // namespacebinding doesn't take "", we convert to null.
      null
    }

    val maybelocalName =
      if (localName.nonEmpty) {
        One(localName)
      } else if (qName.nonEmpty) {
        One(qNameArr.last)
      } else {
        Nope
      }

    val maybeNamespaceURI =
      if (uri.nonEmpty) {
        One(uri)
      } else if (prefixMapping != null) {
        try {
          One(prefixMapping.getURI(qNamePrefix))
        } catch {
          case _: NullPointerException => Nope
        }
      } else {
        Nope
      }

    batchedInfosetEvents(currentIndex).localName = maybelocalName
    batchedInfosetEvents(currentIndex).namespaceURI = maybeNamespaceURI
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
