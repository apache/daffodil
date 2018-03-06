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

import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.dpath.NodeInfo

import scala.xml.Elem
import scala.xml.Text
import scala.xml.Node
import scala.xml.ProcInstr
import scala.xml.Comment
import scala.xml.Atom

class ScalaXMLInfosetInputter(rootNode: Node)
  extends InfosetInputter {

  /**
   * This stack represents the stack of elements that have been visited. Each
   * item on the stack is a tuple of an element and an iterator of its
   * children. The current element is the _1 of the item on the top of the
   * stack. Each time we want to descend into a child element (via a call to
   * next()) we get the next node from _2 on the top of the stack, and the push
   * a tuple of that element and its children to the top of the stack. When a
   * child iterator.hasNext is false, that means we have visited all the
   * children and we should do a end event for the current element at the top
   * of the stack. The doStartEvent variable defined below helps to keep track
   * if we should do a start or end event for the element at the top of the
   * stack. Note that a null _1 at the top of the stack is used to signify the
   * Start/EndDocument events.
   */
  private val stack = {
    val s = new MStackOf[(Elem, Iterator[Node])]

    val iter = rootNode match {
      case e: Elem => e.iterator
      case _ => throw new InvalidInfosetException("Root node is not an element")
    }
    s.push((null, iter))
    s
  }

  private var doStartEvent = true

  override def getEventType(): InfosetInputterEventType = {
    if (stack.top._1 == null) {
      if (doStartEvent) StartDocument else EndDocument
    } else {
      if (doStartEvent) StartElement else EndElement
    }
  }

  override def getLocalName(): String = stack.top._1.label

  override val supportsNamespaces = true

  override def getNamespaceURI(): String = stack.top._1.namespace

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    val text =
      if (stack.top._2.hasNext) {
        val child = stack.top._2.next
        if (stack.top._2.hasNext) {
          throw new NonTextFoundInSimpleContentException(stack.top._1.label)
        }
        val text = child match {
          case t: Text => t.data
          //
          // Note: may be a bug in scala.xml library, but sometimes we are
          // getting Atom[String] here, not Text. Some kinds of things we think
          // of as text, like PCData nodes, are not derived from Text, but from
          // Atom[String], so perhaps that is why.
          //
          // The above issue may be related to our use of the
          // scala.xml.parsing.ConstructingParser which preserves PCData nodes
          // well, and doesn't coalesce them into Text nodes incorrectly like
          // the regular XML loader (in scala.xml 1.0.6) does.
          //
          case a: Atom[_] => a.data.toString()
          case _ => throw new NonTextFoundInSimpleContentException(stack.top._1.label)
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

  override def isNilled(): MaybeBoolean = {
    val elem = stack.top._1
    val nilAttrValueOpt = elem.attribute(XMLUtils.XSI_NAMESPACE, "nil")
    val res =
      if (nilAttrValueOpt.isEmpty) {
        MaybeBoolean.Nope
      } else {
        val nilAttrValueSeq = nilAttrValueOpt.get
        if (nilAttrValueSeq.length > 1) {
          throw new InvalidInfosetException("multiple xsi:nil properties for element " + elem.label)
        }
        val nilAttrValue = nilAttrValueSeq.head.toString
        if (nilAttrValue == "true" || nilAttrValue == "1") {
          MaybeBoolean(true)
        } else if (nilAttrValue == "false" || nilAttrValue == "0") {
          MaybeBoolean(false)
        } else {
          throw new InvalidInfosetException("xsi:nil property is not a valid boolean: '" + nilAttrValue + "' for element " + elem.label)
        }
      }
    res
  }

  override def hasNext(): Boolean = {
    // there is a next event when we are not at the DocumentEnd event. A
    // DocumentEnd event happens if doStartEvent is false and the element at
    // the top of the stack is a null
    val atDocumentEnd = stack.top._1 == null && doStartEvent == false
    !atDocumentEnd
  }

  override def fini = {
    stack.clear
  }

  /**
   * This attemptes to descend into a child by looking for an Element in the
   * child list. This ignores all empty text nodes, processing instructions,
   * and comments. If it is all ignored, then we did not descened
   */
  private def tryDescend(): Boolean = {
    var descended = false
    while (stack.top._2.hasNext && !descended) {
      stack.top._2.next match {
        case e: Elem => {
          // found a child element, push it and its children to the stack
          stack.push((e, e.child.iterator))
          descended = true
        }
        case t: Text if t.data.forall(_.isWhitespace) => // ignore empty text elements
        case _: ProcInstr => // ignore
        case _: Comment => // ignore
        case c => throw new IllegalContentWhereEventExpected("Found " + c.getClass.getName)
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
