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

import scala.util.Try

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DFDL.DaffodilUnhandledSAXException
import org.apache.daffodil.api.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.InfosetInputterEventType.EndDocument
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.XMLUtils

/**
 * The SAXInfosetInputter consumes SAXInfosetEvent objects from the DaffodilUnparseContentHandler
 * class and converts them to events that the DataProcessor unparse can use. This class contains two
 * SAXInfosetEvent objects, the current event the unparse method is processing and the next event
 * to be processed later.
 *
 * This class together with the DaffodilUnparseContentHandler use coroutines to ensure that only one event,
 * at a time, is passed between the two classes. The following is the general process:
 *
 * - the run method is called, with a StartDocument event already loaded on the inputter's queue.
 * This is collected and stored in the currentEvent member
 * - The dp.unparse method is called, and it calls hasNext to make sure an event exists to be
 * processed and then queries the currentEvent. The hasNext call also queues the nextEvent by
 * transferring control to the contentHandler so it can load the next event.
 * - After it is done with the currentEvent, it calls inputter.next to get the next event, and that
 * copies the queued nextEvent into the currentEvent
 * - This process continues until the currentEvent contains an EndDocument event, at which point, the
 * nextEvent is cleared, endDocumentReceived is set to true and hasNext will return false
 * - This ends the unparse process, and the unparseResult and/or any Errors are set on the event. We
 * call resumeFinal passing along that element, terminating this thread and resuming the
 * contentHandler for the last time.
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
  private lazy val currentEvent: DFDL.SAXInfosetEvent = new DFDL.SAXInfosetEvent
  private lazy val nextEvent: DFDL.SAXInfosetEvent = new DFDL.SAXInfosetEvent

  override def getEventType(): InfosetInputterEventType = currentEvent.eventType.orNull

  override def getLocalName(): String = currentEvent.localName.orNull

  override def getNamespaceURI(): String = currentEvent.namespaceURI.orNull

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    val res = if (currentEvent.simpleText.isDefined) {
      currentEvent.simpleText.get
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
    val _isNilled = if (currentEvent.nilValue.isDefined) {
      val nilValue = currentEvent.nilValue.get
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
    if (endDocumentReceived) false
    else if (!nextEvent.isEmpty) true
    else {
      val event = this.resume(unparseContentHandler, Try(currentEvent))
      copyEvent(source = event, dest = nextEvent)
      true
    }
  }

  override def next(): Unit = {
    if (hasNext()) {
      copyEvent(source = Try(nextEvent), dest = currentEvent)
      nextEvent.clear()
      if (currentEvent.eventType.contains(EndDocument)) endDocumentReceived = true
    } else {
      // we should never call next() if hasNext() is false
      Assert.abort()
    }
  }

  def copyEvent(source: Try[DFDL.SAXInfosetEvent], dest: DFDL.SAXInfosetEvent): Unit= {
    if (source.isFailure) dest.clear()
    else {
      val src = source.get
      dest.eventType = src.eventType
      dest.namespaceURI = src.namespaceURI
      dest.localName = src.localName
      dest.nilValue = src.nilValue
      dest.simpleText = src.simpleText
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

  override def fini: Unit = {
    // do nothing
  }

  override protected def run(): Unit = {
    try {
      // startDocument kicks off this entire process, so it should be on the queue so the
      // waitForResume call can grab it. That is set to our current event, so when hasNext is called
      // the nextEvent after the StartDocument can be queued
      copyEvent(source = this.waitForResume(), dest = currentEvent)
      val unparseResult = dp.unparse(this, output)
      currentEvent.unparseResult = One(unparseResult)
      if (unparseResult.isError) {
        // unparseError is contained within unparseResult
        currentEvent.causeError = One(new DaffodilUnparseErrorSAXException(unparseResult))
      }
    } catch {
      case e: Exception => {
        currentEvent.causeError = One(new DaffodilUnhandledSAXException(e.getMessage, e))
      }
    } finally {
      this.resumeFinal(unparseContentHandler, Try(currentEvent))
    }
  }
}
