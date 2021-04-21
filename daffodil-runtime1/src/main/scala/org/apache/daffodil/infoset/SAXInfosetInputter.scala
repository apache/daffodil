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

import java.net.URI
import java.net.URISyntaxException

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DFDL.DaffodilUnhandledSAXException
import org.apache.daffodil.api.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.api.DFDL.SAXInfosetEvent
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.InfosetInputterEventType.EndDocument
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.XMLUtils

/**
 * The SAXInfosetInputter consumes SAXInfosetEvent objects from the DaffodilUnparseContentHandler
 * class and converts them to events that the DataProcessor unparse can use. This class contains an
 * array of batched SAXInfosetEvent objects that it receives from the contentHandler and the index
 * of the current element being processed.
 *
 * This class, together with the SAXInfosetInputter, uses coroutines to ensure that a batch of events
 * (based on the tunable saxUnparseEventBatchSize) can be passed from the former to the latter.
 * The following is the general process:
 *
 * - the run method is called, with the first batch of events, starting with the StartDocument event,
 * already loaded on the inputter's queue.
 * This is collected and stored in the batchedInfosetEvents member, and the currentIndex is set to 0
 * - The dp.unparse method is called, and it calls hasNext to make sure an event exists to be
 * processed and then queries the event at currentIndex. The hasNext call also checks that there is
 * a next event to be processed (currentIndex+1), and if not, queues the next batch of events by
 * transferring control to the contentHandler so it can load them.
 * - After it is done with the current event, it calls inputter.next to get the next event, and that
 * increments the currentIndex and cleans out the event at the previous index
 * - This process continues until the event at currentIndex either contains an EndDocument event or
 * the currentIndex is the last in the batch. If it is the former, the endDocumentReceived flag is
 * set to true and hasNext will return false. If it is the latter, the next batch of events will be
 * queued by transferring control to the contentHandler so it can load them.
 * - This ends the unparse process, and the unparseResult and/or any Errors are set on a single element
 * array containing response events. We call resumeFinal passing along that array, terminating this
 * thread and resuming the contentHandler for the last time.
 *
 * @param unparseContentHandler producer coroutine that sends the SAXInfosetEvent to this class
 * @param dp dataprocessor that we use to kickstart the unparse process and that consumes the
 *           currentEvent
 * @param output  outputChannel of choice where the unparsed data is stored
 */
class SAXInfosetInputter(
  unparseContentHandler: DFDL.DaffodilUnparseContentHandler,
  dp: DFDL.DataProcessor,
  output: DFDL.Output)
  extends InfosetInputter with DFDL.ConsumerCoroutine {

  /**
   * allows support for converting relative URIs in Blobs to absolute URIs, this is necessary
   * for running TDML tests as they allow relative URIs. Because Daffodil proper only uses
   * absolute URIs, we hide this functionality behind this flag. It can be set to true by calling
   * the unparseContentHandler.enableInputterResolutionOfRelativeInfosetBlobURIs(), which calls the
   * inputter's enableResolutionOfRelativeInfosetBlobURIs() function to set the below variable to true
   */
  private var resolveRelativeInfosetBlobURIs: Boolean = false

  private var endDocumentReceived = false
  private var currentIndex: Int = 0
  private var batchedInfosetEvents: Array[SAXInfosetEvent] = _
  private lazy val returnedInfosetEvent: Array[SAXInfosetEvent] = new Array[SAXInfosetEvent](1)

  override def getEventType(): InfosetInputterEventType = batchedInfosetEvents(currentIndex).eventType.orNull

  override def getLocalName(): String = batchedInfosetEvents(currentIndex).localName.orNull

  override def getNamespaceURI(): String = batchedInfosetEvents(currentIndex).namespaceURI.orNull

  override def getSimpleText(primType: NodeInfo.Kind, runtimeProperties: java.util.Map[String, String]): String = {
    val res = if (batchedInfosetEvents(currentIndex).simpleText.isDefined) {
      batchedInfosetEvents(currentIndex).simpleText.get
    } else {
      throw new NonTextFoundInSimpleContentException(getLocalName())
    }
    primType match {
      case _: NodeInfo.String.Kind =>
        val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(res)
        remapped
      case _: NodeInfo.AnyURI.Kind if resolveRelativeInfosetBlobURIs && res.nonEmpty =>
        val absUri = resolveRelativeBlobURIs(res)
        absUri
      case _ =>
        res
    }
  }

  override def isNilled(): MaybeBoolean = {
    val _isNilled = if (batchedInfosetEvents(currentIndex).nilValue.isDefined) {
      val nilValue = batchedInfosetEvents(currentIndex).nilValue.get
      if (nilValue == "true" || nilValue == "1") {
        MaybeBoolean(true)
      } else if (nilValue == "false" || nilValue == "0") {
        MaybeBoolean(false)
      } else {
        throw new InvalidInfosetException("xsi:nil property is not a valid boolean: '" + nilValue +
          "' for element " + getLocalName())
      }
    } else {
      MaybeBoolean.Nope
    }
    _isNilled
  }

  override def hasNext(): Boolean = {
    val nextIndex = currentIndex + 1
    if (endDocumentReceived) {
      // if the current Element is EndDocument, then there is no valid next
      false
    } else if (batchedInfosetEvents != null && nextIndex < batchedInfosetEvents.length) {
      // if we have not yet reached the end of the array and endDocument has not yet been received
      true
    }  else  {
      // there is no nextEvent or it was empty, but we still have no EndDocument. So load the next
      // batch from the contentHandler
      returnedInfosetEvent(0) = batchedInfosetEvents(currentIndex)
      batchedInfosetEvents = this.resume(unparseContentHandler, returnedInfosetEvent)
      // we reset the index to 0 to guarantee that the last element we were looking at when hasNext
      // was called is still the event we'll be looking at, when we leave this function. This is
      // guaranteed because the DaffodilUnparseContentHandler moves the last element into the first
      // index when it resumed.
      currentIndex = 0
      true
    }
  }

  override def next(): Unit = {
    if (hasNext()) {
      // clear element at current index as we're done with it, except in the case we just loaded the
      // new elements, then do nothing
      batchedInfosetEvents(currentIndex).clear()

      // increment current index to the next index
      currentIndex += 1

      // check if new current index is EndDocument
      if (batchedInfosetEvents(currentIndex).eventType.contains(EndDocument)) {
        endDocumentReceived = true
      }
    } else {
      Assert.abort()
    }
  }

  def enableResolutionOfRelativeInfosetBlobURIs(): Unit = resolveRelativeInfosetBlobURIs = true

  /**
   * TDML files must allow blob URI's to be relative, but Daffodil
   * requires them to be absolute with a scheme. So search for the file
   * using TDML semantics and convert to an absolute URI. This function will
   * only be called if resolveRelativeInfosetBlobURIs is true
   */
  private def resolveRelativeBlobURIs(res: String): String = {
    try {
      val uri = new URI(res)
      if (!uri.getPath.startsWith("/")) {
        val abs = Misc.searchResourceOption(uri.getPath, None)
        abs.get.toString
      } else {
        res
      }
    } catch {
      case _: URISyntaxException => res
    }
  }

  override val supportsNamespaces: Boolean = true

  override def fini(): Unit = {
    // do nothing
  }

  override protected def run(): Unit = {
    try {
      // startDocument kicks off this entire process, so the first batch of events, of which
      // startDocument is first, should be on the queue so the waitForResume call can grab it.
      // This populates the inputter.batchedInfosetEvents var for use by the rest of the Inputter
      batchedInfosetEvents = this.waitForResume()
      val unparseResult = dp.unparse(this, output)
      batchedInfosetEvents(currentIndex).unparseResult = One(unparseResult)
      if (unparseResult.isError) {
        // unparseError is contained within unparseResult
        batchedInfosetEvents(currentIndex).causeError = One(new DaffodilUnparseErrorSAXException(unparseResult))
      }
    } catch {
      case e: Exception => {
        batchedInfosetEvents(currentIndex).causeError = One(new DaffodilUnhandledSAXException(e.getMessage, e))
      }
    } finally {
      returnedInfosetEvent(0) = batchedInfosetEvents(currentIndex)
      this.resumeFinal(unparseContentHandler, returnedInfosetEvent)
    }
  }
}
