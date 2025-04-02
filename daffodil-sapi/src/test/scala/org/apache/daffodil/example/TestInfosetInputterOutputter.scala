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

package org.apache.daffodil.example

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.lib.util.MaybeBoolean
import org.apache.daffodil.runtime1.iapi.InfosetArray
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.EndDocument
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.EndElement
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.StartDocument
import org.apache.daffodil.runtime1.infoset.InfosetInputterEventType.StartElement
import org.apache.daffodil.sapi.infoset.InfosetInputter
import org.apache.daffodil.sapi.infoset.InfosetOutputter

case class TestInfosetEvent(
  eventType: InfosetInputterEventType,
  localName: String = null,
  namespaceURI: String = null,
  simpleText: String = null,
  isNilled: MaybeBoolean = MaybeBoolean.Nope
)

object TestInfosetEvent {

  def startDocument() =
    TestInfosetEvent(StartDocument)

  def startComplex(
    name: String,
    namespace: String,
    isNilled: MaybeBoolean = MaybeBoolean.Nope
  ) =
    TestInfosetEvent(StartElement, name, namespace, null, isNilled)

  def startSimple(
    name: String,
    namespace: String,
    text: String,
    isNilled: MaybeBoolean = MaybeBoolean.Nope
  ) =
    TestInfosetEvent(StartElement, name, namespace, text, isNilled)

  def endComplex(name: String, namespace: String) =
    TestInfosetEvent(EndElement, name, namespace)

  def endSimple(name: String, namespace: String) =
    TestInfosetEvent(EndElement, name, namespace)

  def endDocument() =
    TestInfosetEvent(EndDocument)
}

case class TestInfosetInputter(events: TestInfosetEvent*) extends InfosetInputter {

  var curEventIndex = 0

  override def getEventType(): InfosetInputterEventType = events(curEventIndex).eventType
  override def getLocalName(): String = events(curEventIndex).localName
  override def getNamespaceURI(): String = events(curEventIndex).namespaceURI
  override def getSimpleText(primType: NodeInfo.Kind) = events(curEventIndex).simpleText
  override def isNilled(): MaybeBoolean = events(curEventIndex).isNilled

  override def hasNext(): Boolean = curEventIndex + 1 < events.length
  override def next(): Unit = curEventIndex += 1

  override val supportsNamespaces = true

  override def fini(): Unit = {}
}

case class TestInfosetOutputter() extends InfosetOutputter {

  val events = new ArrayBuffer[TestInfosetEvent]()

  override def reset(): Unit = {
    events.clear()
  }

  override def startDocument(): Unit = {
    events.append(TestInfosetEvent.startDocument())
  }

  override def endDocument(): Unit = {
    events.append(TestInfosetEvent.endDocument())
  }

  override def startSimple(simple: InfosetSimpleElement): Unit = {
    events.append(
      TestInfosetEvent.startSimple(
        simple.metadata.name,
        simple.metadata.namespace,
        simple.getText,
        if (simple.metadata.isNillable) MaybeBoolean(simple.isNilled) else MaybeBoolean.Nope
      )
    )
  }

  override def endSimple(simple: InfosetSimpleElement): Unit = {
    events.append(
      TestInfosetEvent.endSimple(simple.metadata.name, simple.metadata.namespace)
    )
  }

  override def startComplex(complex: InfosetComplexElement): Unit = {
    events.append(
      TestInfosetEvent.startComplex(
        complex.metadata.name,
        complex.metadata.namespace,
        if (complex.metadata.isNillable) MaybeBoolean(complex.isNilled) else MaybeBoolean.Nope
      )
    )
  }

  override def endComplex(complex: InfosetComplexElement): Unit = {
    events.append(
      TestInfosetEvent.endComplex(complex.metadata.name, complex.metadata.namespace)
    )
  }

  override def startArray(array: InfosetArray): Unit = {}

  override def endArray(array: InfosetArray): Unit = {}
}
