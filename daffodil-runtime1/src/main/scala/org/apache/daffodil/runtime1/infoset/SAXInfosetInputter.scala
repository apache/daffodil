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

import java.lang.{ Boolean => JBoolean }
import java.net.URI
import java.net.URISyntaxException

import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType
import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType._
import org.apache.daffodil.api.infoset.{ InfosetInputter => JInfosetInputter }
import org.apache.daffodil.api.{ DataProcessor => JDataProcessor }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Coroutine
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.iapi.DFDL.UnparseResult
import org.apache.daffodil.runtime1.processors.DaffodilUnparseContentHandlerImpl

/**
 * The SAXInfosetInputter worker coroutine receives batches of SAXInfosetEvent
 * objects from a DaffodilUnparseContentHandlerImpl main coroutine and calls
 * the unparse() function to provide these events and unparse the data.
 * 
 * See the DaffodilUnparseContentHandlerImpl for a detailed description of how
 * these two classes interact.
 *
 * @param unparseContentHandler main coroutine that sends the SAXInfosetEvents to this class
 * @param dp DataProcessor that we use to start the unparse and provide infoset information
 * @param output  OutputChannel where the unparsed data is written
 * @param resolveRelaiveInfosetBlobURIs if true, elements with type xs:anyURI type and a
 *   relativeURI are resolved relative to the classpath. This should only be true when used
 *   via a TDMLRunner or similar testing infrastructure
 */
class SAXInfosetInputter(
  unparseContentHandler: DaffodilUnparseContentHandlerImpl,
  dp: JDataProcessor,
  output: DFDL.Output,
  resolveRelativeInfosetBlobURIs: Boolean
) extends JInfosetInputter
  with Coroutine[Array[SAXInfosetEvent]] {

  /**
   * The index into the batchedInfosetEvents array that the InfosetInputter is
   * currently providing information to unparse()
   */
  private var currentIndex: Int = 0

  /**
   * The array of batched infoset events received from the ContentHandler.
   */
  private var batchedInfosetEvents: Array[SAXInfosetEvent] = _

  /**
   * A helper function to succinctly access the SAXInfosetEvent at the currentIndex
   * in the batchedInfosetEvents array. This is the event that the InfosetInputter is
   * currently returning information about to the unparse().
   */
  @inline
  private def currentEvent = batchedInfosetEvents(currentIndex)

  override def getEventType(): InfosetInputterEventType = currentEvent.eventType.get

  override def getLocalName(): String = currentEvent.localName.get

  override def getNamespaceURI(): String = currentEvent.namespaceURI.orNull

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    val res = if (currentEvent.simpleText.isDefined) {
      currentEvent.simpleText.get
    } else {
      throw new NonTextFoundInSimpleContentException(getLocalName())
    }
    if (primType eq NodeInfo.String) {
      val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(res)
      remapped
    } else if (
      resolveRelativeInfosetBlobURIs && (primType eq NodeInfo.AnyURI) && res.nonEmpty
    ) {
      val absUri = resolveRelativeBlobURIs(res)
      absUri
    } else {
      res
    }
  }

  override def isNilled(): Option[JBoolean] = {
    val _isNilled: Option[JBoolean] = if (currentEvent.nilValue.isDefined) {
      val nilValue = currentEvent.nilValue.get
      if (nilValue == "true" || nilValue == "1") {
        Some(true)
      } else if (nilValue == "false" || nilValue == "0") {
        Some(false)
      } else {
        throw new InvalidInfosetException(
          "xsi:nil property is not a valid boolean: '" + nilValue +
            "' for element " + getLocalName()
        )
      }
    } else {
      None
    }
    _isNilled
  }

  override def hasNext(): Boolean = {
    // If we haven't reached an EndDocument event yet, there must be more
    // events on their way, even if we don't know for sure yet.
    currentEvent.eventType.get ne EndDocument
  }

  override def next(): Unit = {
    Assert.usage(hasNext())

    // we are about to move to the next event, clear the current event so it is
    // already reset when the DaffodilUnparseContentHandlerImpl goes to reuse
    // it later
    currentEvent.clear()

    if (currentIndex == batchedInfosetEvents.length - 1) {
      // We have used all of the events that have been batched, and still have not
      // seen an EndDocument event, so there must be more events. We send None back
      // to the main coroutine to signify that there isn't an error and we aren't
      // finished, but we do need more events.
      batchedInfosetEvents = this.resume(unparseContentHandler, Nope)

      // We are now back from the main coroutine and received a new batch of events.
      // We set the index back to the beginning of the batchedInfosetEvents array and
      // will start returning information from these for unparse
      currentIndex = 0
    } else {
      // We are still reading our current batch of infoset events, just increment the
      // current index to the next index
      currentIndex += 1
    }

    if (currentEvent.mixedContent.isDefined) {
      // This is a Start or EndElement event that has mixed content prior to
      // it. We must throw an exception here, which causes unparse() to finish
      // and return execution back to the DaffodilUnparseContentHandlerImpl
      // main coroutine where it can report the error via the SAX API
      val eventType = if (currentEvent.eventType.get eq StartElement) "start" else "end"
      val element = s"{${currentEvent.namespaceURI.get}}${currentEvent.localName.get}"
      val msg = s"Mixed content found prior to $eventType of $element"
      throw new IllegalContentWhereEventExpected(msg)
    }
  }

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

  override def getSupportsNamespaces: Boolean = true

  override def fini(): Unit = {
    // do nothing
  }

  override protected def run(): Unit = {
    try {
      // We are starting the inputter worker routine in a new Thread, the first
      // thing we need to do is wait to receive the batch of events from the
      // DaffodilUnparseContentHandlerImpl main coroutine. Once received, we
      // can call unparse() which will call the above InfosetInputter functions
      // to read these events and ask the main routine for more batched events
      // if needed
      batchedInfosetEvents = this.waitForResume()
      val unparseResult = dp.unparse(this, output).asInstanceOf[UnparseResult]

      // now that the unparse is complete, we can send the unparse result back to
      // the main coroutine.
      this.resumeFinal(unparseContentHandler, One(Right(unparseResult)))
    } catch {
      // $COVERAGE-OFF$
      case e: Exception => {
        // if any Exception makes it out of the unparse call, that's almost certainly
        // a bug. We capture that here and return it back to the main coroutine where
        // it can rethrow it so that the main thread sees it
        this.resumeFinal(unparseContentHandler, One(Left(e)))
      }
      // $COVERAGE-ON$
    }
  }
}

class SAXInfosetEvent() {
  var localName: Maybe[String] = Nope
  var simpleText: Maybe[String] = Nope
  var namespaceURI: Maybe[String] = Nope
  var eventType: Maybe[InfosetInputterEventType] = Nope
  var nilValue: Maybe[String] = Nope
  var mixedContent: Maybe[String] = Nope

  def clear(): Unit = {
    localName = Nope
    simpleText = Nope
    namespaceURI = Nope
    eventType = Nope
    nilValue = Nope
    mixedContent = Nope
  }

  def isEmpty: Boolean = {
    localName.isEmpty &&
    simpleText.isEmpty &&
    namespaceURI.isEmpty &&
    eventType.isEmpty &&
    nilValue.isEmpty &&
    mixedContent.isEmpty
  }

  override def toString: String = {
    s"${if (eventType.isDefined) eventType.get else "No_EventType"}:" +
      s"{${if (namespaceURI.isDefined) namespaceURI.get else ""}}" +
      s"${if (localName.isDefined) localName.get else "No_Name"}:" +
      s"(${if (simpleText.isDefined) simpleText.get else "No_content"})"
  }
}
