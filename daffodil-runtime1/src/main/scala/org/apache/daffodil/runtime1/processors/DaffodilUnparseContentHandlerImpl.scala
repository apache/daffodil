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

package org.apache.daffodil.runtime1.processors

import javax.xml.XMLConstants
import scala.xml.NamespaceBinding
import scala.xml.TopScope

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MainCoroutine
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnhandledSAXException
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.EndDocument
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.EndElement
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.StartDocument
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.StartElement
import org.apache.daffodil.runtime1.infoset.SAXInfosetEvent
import org.apache.daffodil.runtime1.infoset.SAXInfosetInputter

import org.xml.sax.Attributes
import org.xml.sax.Locator

/**
 * Handle and unparse XMLReader SAX events using a provided DataProcessor and
 * OutputChannel
 *
 * Note: XMLReaders using this as their ContentHandler must have support for XML
 * namespaces so that we provided namespace URI and prefix information that Daffodil
 * requires to unparse.
 *
 * The SAX ContentHandler API is push-based, but the Daffodil InfosetInputter unparse
 * API is pull-based, so these two API's are at odds with one another. To link the
 * two, we create two classes that implement a coroutine-like API to communicate and
 * ensure that the push and pull sides of the two APIs never run at the same time
 * (see Coroutine.scala for implementation details). The main coroutine or "event
 * queuer" is this DaffodilUnparseContentHandlerImpl and runs on the same thread as
 * an XMLReader to handle and batch SAX events. The worker coroutine or "event
 * puller" is an instance of the SAXInfosetInputter which calls the actual unparse()
 * function to query these batched SAX events and unparse data.
 *
 * Below is a description of how this class is used and they communicate to unparse
 * using the SAX XMLReader API:
 *
 * 1. A DaffodilUnparseContentHandlerImpl instance is created, which initializes a
 *    SAXInfosetInputter instance.
 *
 * 2. An XMLReader instance is created and configured to use the
 *    DaffodilUnparseContentHandlerImpl to handle its SAX events.
 *
 * 3. The DaffodilUnparseContentHandlerImpl handles events from the XMLReader,
 *    gathers the necessary information from those events, and fills out an array of
 *    SAXInfosetEvent objects, called a "batch".
 *
 * 4. When a full batch has been gathered, or an endDocument() event is handled, the
 *    DaffodilUnparseContentHandlerImpl calls resume() to pause execution and start
 *    the SAXInfosetInputter coroutine, sending it the batch of events.
 *
 * 5. Since this is the first time the SAXInfosetInputter has been given control, its
 *    coroutine Thread is started and its run() function called. This calls
 *    waitResume() to receive the first batch of events. It then calls the
 *    DataProcessor unparse() function to begin the unparse.
 *
 * 6. The SAXInfosetInputter functions are called by the unparse, which reads the
 *    batched events and provides the necessary information to unparse the infoset.
 *    Once the SAXInfosetInputter has unparsed all batched events and needs a new
 *    event in the next() function, it calls resume() to pause execution and resume
 *    the DaffodilUnparseContentHandlerImpl coroutine, sending it Nope to signify it
 *    needs more events.
 *
 * 7. The DaffodilUnparseContentHandlerImpl resumes and again continues to handle SAX
 *    events and fill out the batched events array until it is full or endDocument()
 *    is handled, at which point it calls resume() to pause and send the new batch of
 *    events to the SAXInfosetInputter where it resumes control.
 *
 * 8. Steps 6 and 7 repeat until the SAXInfosetInputter signals back to the
 *    DaffodilUnparseContentHandlerImpl that it is complete calling resumeFinal() and
 *    sending a One object containing either an UnparseResult or an Exception. At
 *    this point, the SAXInfosetInputter is complete and provide control back to the
 *    DaffodilUnparseContentHandlerImpl--the SAXInfosetInputter coroutine is done.
 *
 * 9. The DaffodilUnparseContentHandlerImpl resumes, recieves and examines the result
 *    from the SAXInfosetInputter, and either makes the UnparseResult available to
 *    the XMLReader or throws a SAXException if there was an error.
 *
 * @param dp DataProcessor object that will be used to start the unparse
 * @param output OutputChannel of where the unparsed data is written
 */
class DaffodilUnparseContentHandlerImpl(dp: DFDL.DataProcessor, output: DFDL.Output)
  extends MainCoroutine[Maybe[Either[Exception, DFDL.UnparseResult]]]
  with DFDL.DaffodilUnparseContentHandler {

  /**
    * The SAXInfosetInputter worker coroutine that receives the batches of
    * SAXInfosetEvents to be used to unparse data. The coroutine thread will not be
    * created until the DaffodilUnparseContentHandlerImpl calls resume() with the
    * first batch of events
    */
  private lazy val inputter: SAXInfosetInputter =
    new SAXInfosetInputter(this, dp, output, resolveRelativeInfosetBlobURIs)

  /**
   * Buffer to accumulate characters passed to the characters() event handler
   */
  private lazy val characterData = new StringBuilder

  /**
   * Used to store the current in-scope prefix namespace mappings, determined by the
   * start/endPrefixMapping() handlers, or from xmlns Attributes in the
   * startElement() handler.
   */
  private var prefixMapping: NamespaceBinding = TopScope

  /**
   * If the XMLReader doesn't use the start/endPrefixMapping() handlers to pass
   * along namespace mapping information, we must extract the information from the
   * Attributes parameter passed to the startElement() handler. Doing so can
   * potentially add multiple namespace mappings that must be removed in the
   * associated endElement() handler. This stack keeps track of which mappings are
   * added in startElement() so they can be quickly removed in endElement() by
   * popping off the stack.
   */
  private lazy val prefixMappingTrackingStack = new MStackOf[NamespaceBinding]

  /**
   * SAXInfosetEvents are buffered into this array before sent to the
   * SAXInfosetInputter worker coroutine. We preallocate these events and mutate them
   * as needed to avoid allocation overhead. Note that because we use coroutines,
   * there are no concerns about thread safety--there is no way for one class to read
   * this array while another is writing to it.
   */
  private lazy val batchedInfosetEvents: Array[SAXInfosetEvent] = {
    val batchSize = dp.tunables.saxUnparseEventBatchSize
    Assert.invariant(batchSize > 0, "invalid saxUnparseEventBatchSize; minimum value is 1")
    Array.fill[SAXInfosetEvent](batchSize)(new SAXInfosetEvent)
  }

  /**
   * The current index into the batchedInfosetEvents array that we are currently
   * modifying based on SAX XMLReader callback events
   */
  private var currentIndex: Int = 0

  /**
   * A helper function to succinctly access the SAXInfosetEvent at the currentIndex
   * in the batchedInfosetEvents array. This is the current event that the
   * ContentHandler is modifying in preparation to be sent to the SAXInfosetInputter.
   */
  @inline
  private def currentEvent = batchedInfosetEvents(currentIndex)

  /**
   * This flag is set to true when startPrefixMapping() is handled. When true, we
   * make the assumption that we don't need to use the Attributes parameter from
   * startElement() to get the namespace URI information and will solely rely on
   * start/endPrefixMapping(). If false, we will use Attributes to get the
   * namespace URI info.
   */
  private var contentHandlerPrefixMappingUsed = false

  /**
   * Returns null if the UnparseResult has not been determined yet (i.e. the worker
   * coroutine is still unparsing), or if an unexpected exception occurred that
   * prevented unparse from completing. Otherwise returns the UnparseResult.
   */
  def getUnparseResult: DFDL.UnparseResult = unparseResult.orNull

  private var unparseResult: Maybe[DFDL.UnparseResult] = Nope

  /**
   * Enable support for converting relative URIs in xs:anyURI blobs to absolute URIs
   *
   * This should only be needed when running TDML tests since they allow relative
   * URIs in the expected infoset. All other uses of Daffodil should use absolute
   * URIs and should not need to call this function. This function must be called
   * prior to startDocument() being handled to have an affect.
   */
  def enableResolutionOfRelativeInfosetBlobURIs(): Unit = resolveRelativeInfosetBlobURIs = true

  private var resolveRelativeInfosetBlobURIs: Boolean = false

  override def startDocument(): Unit = {
    currentEvent.eventType = One(StartDocument)
    currentEventIsFinished()
  }

  override def endDocument(): Unit = {
    currentEvent.eventType = One(EndDocument)
    currentEventIsFinished()
  }

  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    contentHandlerPrefixMappingUsed = true
    val pre = if (prefix == "") null else prefix
    prefixMapping = NamespaceBinding(pre, uri, prefixMapping)
  }

  /**
   * From the ContentHandler javadoc: start/endPrefixMapping events are not
   * guaranteed to be properly nested relative to each other: all startPrefixMapping
   * events will occur immediately before the corresponding startElement event, and
   * all endPrefixMapping events will occur immediately after the corresponding
   * endElement event, but their order is not otherwise guaranteed.
   *
   * Because the order is not guaranteed but proper nesting is, we can just remove
   * the most recent prefix mapping
   */
  override def endPrefixMapping(prefix: String): Unit = {
    prefixMapping = prefixMapping.parent
  }

  /**
   * Searches the Attributes from a startElement() handler for prefix mappings and
   * adds to the prefixMapping NamspaceBindings
   */
  def addPrefixMappingFromAttributes(atts: Attributes): Unit = {
    var i = 0
    val numAtts = atts.getLength
    while (i < numAtts) {
      val qName = atts.getQName(i)
      if (qName == "xmlns") {
        prefixMapping = NamespaceBinding(null, atts.getValue(i), prefixMapping)
      } else if (qName.startsWith("xmlns:")) {
        val prefix = qName.substring(6)
        prefixMapping = NamespaceBinding(prefix, atts.getValue(i), prefixMapping)
      } else {
        // do nothing, not a namespace mapping attribute
      }
      i += 1
    }
  }

  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    atts: Attributes
  ): Unit = {
    // If we are handling a startElement() event and the current eventType is
    // defined, it must mean that the currentEvent is a StartElement event for a
    // complex element and we are starting its first child element. We are done with
    // the currentEvent
    if (currentEvent.eventType.isDefined) {
      Assert.invariant(currentEvent.eventType.get eq StartElement)
      currentEventIsFinished()
    }

    // Begin setting information for the new StartElement event
    currentEvent.eventType = One(StartElement)

    // If the XMLReader doesn't use the start/endPrefixMapping() handlers to pass
    // namespace mapping information, we must extract the information from the
    // Attributes parameter. We push the current mapping to a stack first so that
    // when the associated endElement() event is handled we can remove whatever
    // mappings we added (which could be nothing). Note that this must happen before
    // setLocalNameAndNamespaceUri since this startElement may use namespace prefixes
    // defined in the Attributes.
    if (!contentHandlerPrefixMappingUsed) {
      prefixMappingTrackingStack.push(prefixMapping)
      addPrefixMappingFromAttributes(atts)
    }

    // Use Attributes to determine xsi:nil value. Note that we don't care if the
    // value of the attribute is valid yet. We let the SAXInfosetInputter check that
    // and throw an exception to end the unparse cleanly.
    val nilValue = atts.getValue(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil")
    if (nilValue != null) {
      currentEvent.nilValue = One(nilValue)
    }

    // set localName and namespaceURI
    setLocalNameAndNamespaceUri(uri, localName, qName)

    // check for any mixed content prior to this StartElement
    checkMixedContent()
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    if (currentEvent.eventType.contains(StartElement)) {
      // if the current event is a StartElement event, then we are just ending a
      // simple element. Any characterData that we have accumulated so far via
      // characters() events becomes the simpleText of that StartElement. And we are
      // now done with that event so we can clear the character state and declare
      // that we are done with this element.
      currentEvent.simpleText = One(characterData.toString)
      characterData.setLength(0)
      currentEventIsFinished()
    } else {
      // This must be ending a complex element. Check for any mixed content
      // between this endElement() and the end of its last child element.
      checkMixedContent()
    }

    // Begin setting information for the new EndElement event
    currentEvent.eventType = One(EndElement)

    // set localName and namespaceURI
    setLocalNameAndNamespaceUri(uri, localName, qName)

    // If the XMLReader doesn't use start/endPrefixMapping() handlers to pass
    // along namespaceMapping information, that means we added mappings from
    // the Attributes in the startElement() handler and pushed those new
    // mappings to a stack. We need to undo those mappings by popping the stack.
    // Note that this must happen after setLocalAndNamespaceUri() since this
    // endElement() might use namespace prefixes defined in the Attributes
    // added in the associated startElement() call.
    if (!contentHandlerPrefixMappingUsed) {
      prefixMapping = prefixMappingTrackingStack.pop
    }

    // We are done with this EndEvent
    currentEventIsFinished()
  }

  /**
   * This function should be called whenever mixed content (i.e non-whitespace
   * characters in between elements) should be checked for and, since it isn't
   * allowed in a DFDL infoset, state mutated to indicate the error.
   *
   * Note that if mixed content is detected we cannot simply throw a SAXException.
   * This is because if this ContentHandler does so, the unparse coroutine thread
   * will still be blocked waiting for more events that will never come, leaving it
   * hanging.
   *
   * Instead, we set the mixedContent value of the current event to the characterData
   * we have accumulated, and the SAXInfosetInputer will check this and error when it
   * gets to this event. We also reset the characterData and just pretend like there
   * was no mixed content as far as the ContentHandler is concerned. Eventually the
   * SAXInfosetInputter will discover the error and cause processing to stop.
   */
  private def checkMixedContent(): Unit = {
    if (characterData.nonEmpty) {
      if (!Misc.isAllWhitespace(characterData)) {
        currentEvent.mixedContent = One(characterData.toString)
      }
      characterData.setLength(0)
    }
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    characterData.appendAll(ch, start, length)
  }

  /**
   * This function should be called when we have gathered up all information for the
   * current SAXInfosetEvent. Because we are batching up events, this may send all
   * batched events (including the one we just finished) to the SAXInfosetInputter
   * coroutine. Whether or not is does so, upon return of this function it will have
   * updated the currentIndex to the index where the next SAX event information
   * should be written. The event at this index is guaranteed to be empty when this
   * function returns.
   *
   * If this receives either an UnparseResult with error status or an Exception from
   * the SAXInfosetInputter coroutine, it will instead throw an appropriate
   * SAXException.
   */
  private def currentEventIsFinished(): Unit = {
    if (
      currentIndex < batchedInfosetEvents.length - 1 && (currentEvent.eventType.get ne EndDocument)
    ) {
      // We have room left on the batchedInfosetEvents array and the current event is
      // not EndDocument. Increment currentIndex so future SAX API calls modify that
      // event state. We also ensure that the SAXInfosetInputter cleared the state of
      // the new index after it was done with it.
      currentIndex += 1
      Assert.invariant(currentEvent.isEmpty)
    } else {
      // We completely fill up the batched events buffer or we have reached the
      // EndDocument event. Send everything we've batched to be unparsed by the
      // SAXInfosetInputter subroutine.
      val maybeUnparseResultOrException = this.resume(inputter, batchedInfosetEvents)

      if (maybeUnparseResultOrException.isEmpty) {
        // no error and not finished, the SAXInfosetInputter just needs more events. The
        // SAXInfoseInputter has cleared all the events, so we can reset our index back to
        // the beginning and batch more SAX events.
        currentIndex = 0
        Assert.invariant(currentEvent.isEmpty)
      } else {
        maybeUnparseResultOrException.get match {
          case Right(ur) => {
            // we finished unparsing and got an UnparseResult. We set the
            // unparseResult variable so the user can retrieve it with the
            // getUnparseResult() function. If the unparse failed, we also throw the
            // UnparseResult as a SAX Exception since that is generally what SAX API
            // users expect on failure
            unparseResult = One(ur)
            if (ur.isError) {
              throw new DaffodilUnparseErrorSAXException(ur)
            }
          }
          // $COVERAGE-OFF$
          case Left(e) => {
            // unparse threw an unexpected exception, this is likely a bug. We don't
            // have an UnparseResult so just rethrow the exception as a SAXException.
            throw new DaffodilUnhandledSAXException(e)
          }
          // $COVERAGE-ON$
        }
      }
    }
  }

  /**
   * Use the prefixMapping and input parameters to set the localName and NamespaceURI of the
   * SAXInfoset object.
   */
  private def setLocalNameAndNamespaceUri(
    uri: String,
    localName: String,
    qName: String
  ): Unit = {
    lazy val qNameArr = qName.split(":")
    lazy val qNamePrefix = if (qNameArr.length > 1) { // there is a prefix
      qNameArr.head
    } else {
      // if there is no prefix, we attempt to try to get the namespace when the prefix is "". Since
      // NamespaceBinding doesn't take "", we convert to null.
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
      } else {
        Maybe(prefixMapping.getURI(qNamePrefix))
      }

    currentEvent.localName = maybelocalName
    currentEvent.namespaceURI = maybeNamespaceURI
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

  override def setDocumentLocator(locator: Locator): Unit = {
    // do nothing
  }

}
