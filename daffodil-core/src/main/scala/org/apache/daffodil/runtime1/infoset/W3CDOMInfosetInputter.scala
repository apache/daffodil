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
import java.util.Optional
import javax.xml.XMLConstants

import org.apache.daffodil.api
import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo

import org.w3c.dom.Comment
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.w3c.dom.ProcessingInstruction
import org.w3c.dom.Text

class W3CDOMInfosetInputter(doc: Document) extends api.infoset.InfosetInputter {

  /**
   * This stack represents the stack of elements that have been visited. Each
   * item on the stack is a tuple of an element and an iterator of its
   * children. The current element is the _1 of the item on the top of the
   * stack. Each time we want to descend into a child element (via a call to
   * next()) we get the next node from _2 on the top of the stack, and then push
   * a tuple of that element and its children to the top of the stack. When a
   * child iterator.hasNext is false, that means we have visited all the
   * children and we should do a end event for the current element at the top
   * of the stack. The doStartEvent variable defined below helps to keep track
   * if we should do a start or end event for the element at the top of the
   * stack. Note that null at the top of the stack is used to signify the
   * Start/EndDocument events. All other items on the stack should be an
   * Element.
   */
  private val stack = {
    val s = new MStackOf[(Element, Iterator[Node])]
    val iterator = new IterableNodeList(doc.getChildNodes)
    if (!iterator.hasNext) {
      throw new InvalidInfosetException("Document does not contain a root element")
    }
    s.push((null, iterator))
    s
  }

  private var doStartEvent = true

  override def getEventType(): InfosetInputterEventType = {
    import InfosetInputterEventType._
    if (stack.top._1 == null) {
      if (doStartEvent) StartDocument else EndDocument
    } else {
      if (doStartEvent) StartElement else EndElement
    }
  }

  override def getLocalName(): String = {
    // If an element has no namespace it was created with the "createElement" method
    // and getLocalName will always return null, so use getNodeName instead
    if (stack.top._1.getNamespaceURI == null) {
      stack.top._1.getNodeName
    } else {
      stack.top._1.getLocalName
    }
  }

  override def getSupportsNamespaces = true

  override def getNamespaceURI(): String = stack.top._1.getNamespaceURI

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    val text =
      if (stack.top._2.hasNext) {
        val child = stack.top._2.next()
        if (stack.top._2.hasNext) {
          throw new NonTextFoundInSimpleContentException(stack.top._1.getNodeName)
        }

        val text = child match {
          case t: Text => t.getWholeText()
          case _ => throw new NonTextFoundInSimpleContentException(stack.top._1.getNodeName)
        }
        if (primType.isInstanceOf[NodeInfo.String.Kind]) {
          XMLUtils.remapPUAToXMLIllegalCharacters(text)
        } else {
          text
        }
      } else {
        ""
      }

    text
  }

  override def isNilled(): Optional[JBoolean] = {
    val elem = stack.top._1
    val nilAttrValue = elem.getAttributeNS(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil")
    val res: Option[JBoolean] =
      if (nilAttrValue == "") {
        None
      } else if (nilAttrValue == "true" || nilAttrValue == "1") {
        Some(true)
      } else if (nilAttrValue == "false" || nilAttrValue == "0") {
        Some(false)
      } else {
        throw new InvalidInfosetException(
          "xsi:nil property is not a valid boolean: '" + nilAttrValue + "' for element " + elem.getNodeName
        )
      }
    res
  }

  override def hasNext(): Boolean = {
    // there is a next event when we are not at the DocumentEnd event. A
    // DocumentEnd event happens if doStartEvent is false and the element at
    // the top of the stack is null.
    val atDocumentEnd = stack.top._1 == null && doStartEvent == false
    !atDocumentEnd
  }

  override def fini() = {
    stack.clear()
  }

  /**
   * This attempts to descend into a child by looking for an Element in
   * the child list. This ignores all empty text nodes, processing
   * instructions, and comments. If it is all ignored, then we did not descend
   */
  private def tryDescend(): Boolean = {

    var descended = false

    while (stack.top._2.hasNext && !descended) {
      stack.top._2.next() match {
        case e: Element => {
          // found a child element, push it and its children to the stack
          stack.push((e, new IterableNodeList(e.getChildNodes)))
          descended = true
        }
        case t: Text if t.getWholeText.forall(_.isWhitespace) => // ignore empty text elements
        case _: ProcessingInstruction => // ignore
        case _: Comment => // ignore
        case n => throw new IllegalContentWhereEventExpected("Found " + n.getNodeName)
      }
    }
    descended
  }

  override def next(): Unit = {
    if (tryDescend()) {
      // found a child element, make sure we do a start event for it next
      doStartEvent = true
    } else {
      // we have finished parsing all the children of this thing
      if (doStartEvent) {
        // this was a simple type with no children to descened into. We already
        // did a startEvent for it, so the next event needs to be the end event
        doStartEvent = false
      } else {
        // we just did a end event for this element, so we're done with it.
        // pop it off the stack, and see if it has any later siblings
        stack.pop
        if (tryDescend()) {
          // found a sibling element, it was pushed to the top of the stack in
          // tryDescend, do start event for it
          doStartEvent = true
        } else {
          // the element we just popped has no later siblings. So we
          // need an end event for the element now at the top of the stack.
          // doStartEvent must be false right here, so its already correct
          // and we don't have to do anything.
        }
      }
    }
  }

}

class IterableNodeList(n: NodeList) extends Iterator[Node] {

  private var index = 0

  override def hasNext: Boolean = {
    n.item(index) != null
  }

  override def next(): Node = {
    val item = n.item(index)
    if (item == null) {
      throw new NoSuchElementException("next on empty iterator")
    }
    index += 1
    item
  }

}
