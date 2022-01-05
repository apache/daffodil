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

import java.io.InputStream

import scala.collection.mutable.ArrayBuffer

import scala.xml.Elem
import scala.xml.SAXParser
import scala.xml.Text
import scala.xml.XML

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.infoset.InfosetInputterEventType._
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.xml.DaffodilSAXParserFactory
import org.apache.daffodil.xml.XMLUtils

object NullInfosetInputter {

  case class Event(
    eventType: InfosetInputterEventType,
    localName: String = null,
    namespaceURI: String = null,
    simpleText: String = null,
    isNilled: MaybeBoolean = MaybeBoolean.Nope,
  )

  def toEvents(is: InputStream): Array[Event] = {
    val elem = {
      val parser: SAXParser = {
        val f = DaffodilSAXParserFactory()
        f.setNamespaceAware(false)
        val p = f.newSAXParser()
        p
      }
      val node = XML.withSAXParser(parser).load(is)
      val normalized = XMLUtils.normalize(node)
      normalized
    }
    val events = ArrayBuffer[Event]()
    events += Event(StartDocument)
    nodeToEvents(elem.asInstanceOf[Elem], events)
    events += Event(EndDocument)
    events.toArray
  }

  /**
   * Recursively walk a Scala XML Node, converting each Elem to an Event and
   * appends it to the events array. Note that this function expects elem to
   * have been normalized with XMLUtils.normalize, such that each Elem either
   * has no children, a single Text child, or one or more Elem children.
   */
  private def nodeToEvents(elem: Elem, events: ArrayBuffer[Event]): Unit = {
    val isSimple = elem.child.length == 0 || elem.child(0).isInstanceOf[Text]
    val localName = elem.label
    val namespaceURI = elem.namespace
    val (simpleText, isNilled) = if (isSimple) {
      val text = XMLUtils.remapPUAToXMLIllegalCharacters(elem.text)
      val isNilled = elem.attribute(XMLUtils.XSI_NAMESPACE, "nil").map { attrs =>
        val str = attrs.head.toString
        val value = str == "true" || str == "1"
        MaybeBoolean(value)
      }.getOrElse(MaybeBoolean.Nope)
      (text, isNilled)
    } else {
      (null, MaybeBoolean.Nope)
    }

    events += Event(StartElement, localName, namespaceURI, simpleText, isNilled)
    if (!isSimple) elem.child.foreach { c => nodeToEvents(c.asInstanceOf[Elem], events) }
    events += Event(EndElement, localName, namespaceURI)
  }
}

/**
 * InfosetInputter that has the minimum possible amount of overhead during
 * unparse operations, intended to be used for performance comparisons. The
 * events array should be created by calling NullInfosetInputter.toEvents()
 * prior to any performance testing and outside any critical sections, and
 * passed into a new NullInfosetInputter for unparsing.
 */
class NullInfosetInputter(events: Array[NullInfosetInputter.Event]) extends InfosetInputter {

  private var curIndex = 0
  private var curEvent: NullInfosetInputter.Event = events(0)

  val supportsNamespaces: Boolean = true

  def getEventType(): InfosetInputterEventType = curEvent.eventType
  def getLocalName(): String = curEvent.localName
  def getNamespaceURI(): String = curEvent.namespaceURI
  def getSimpleText(primType: NodeInfo.Kind, runtimeProperties: java.util.Map[String,String]): String = curEvent.simpleText
  def isNilled(): MaybeBoolean = curEvent.isNilled

  def hasNext(): Boolean = curIndex + 1 < events.length
  def next(): Unit = {
    curIndex += 1
    curEvent = events(curIndex)
  }

  def fini(): Unit = {}
}
