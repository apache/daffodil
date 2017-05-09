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

import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import edu.illinois.ncsa.daffodil.xml.XMLUtils

import org.jdom2.Document
import org.jdom2.Element
import org.jdom2.Text
import org.jdom2.Content
import org.jdom2.Namespace
import org.jdom2.ProcessingInstruction
import org.jdom2.Comment

import java.util.Iterator

object JDOMInfosetInputter {
  protected val JDOM_XSI_NAMESPACE = Namespace.getNamespace(XMLUtils.XSI_NAMESPACE)
}

class JDOMInfosetInputter(doc: Document)
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
   * stack. Note that null at the top of the stack is used to signify the
   * Start/EndDocument events. All other items on the stack should be an
   * Element.
   */
  private val stack = {
    val s = new MStackOf[(Element,Iterator[Content])]
    val docChildren = doc.getContent.iterator
    if (!docChildren.hasNext) {
      throw new InvalidInfosetException("Document does not contain a root element")
    }
    s.push((null, docChildren))
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

  override def getLocalName(): String = stack.top._1.getName
  
  override val supportsNamespaces = true

  override def getNamespaceURI(): String = stack.top._1.getNamespace.getURI
  
  override def getSimpleText(): String = {
    val text =
      if (stack.top._2.hasNext) {
        val child = stack.top._2.next
        if (stack.top._2.hasNext) {
          throw new NonTextFoundInSimpleContentException(stack.top._1.getQualifiedName)
        }

        val text = child match {
          case t: Text => t.getText()
          case _ => throw new NonTextFoundInSimpleContentException(stack.top._1.getQualifiedName)
        }
        XMLUtils.remapPUAToXMLIllegalCharacters(text)
      } else {
        ""
      }

    // a call to getCurrentEventType will be the EndElement for this simple type
    doStartEvent = false

    text
  }

  override def isNilled(): MaybeBoolean = {
    val elem = stack.top._1
    val nilAttrValue = elem.getAttributeValue("nil", JDOMInfosetInputter.JDOM_XSI_NAMESPACE)
    val res =
      if (nilAttrValue == null) {
        MaybeBoolean.Nope
      } else if (nilAttrValue == "true" || nilAttrValue == "1") {
        MaybeBoolean(true)
      } else if (nilAttrValue == "false" || nilAttrValue == "0") {
        MaybeBoolean(false)
      } else {
        throw new InvalidInfosetException("xsi:nil property is not a valid boolean: '" + nilAttrValue + "' for element " + elem.getQualifiedName)
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

  override def fini = {
    stack.clear
  }

  /**
   * This attemptes to descend into a child by looking for an Element in
   * the child list. This ignores all empty text nodes, processing
   * instructions, and comments. If it is all ignored, then we did not descened
   */
  private def tryDescend(): Boolean = {
    var descended = false
    while (stack.top._2.hasNext && !descended) {
      stack.top._2.next match {
        case e: Element => {
          // found a child element, push it and its children to the stack
          stack.push((e, e.getContent.iterator))
          descended = true
        }
        case t: Text if t.getText().forall(_.isWhitespace) => // ignore empty text elements
        case _: ProcessingInstruction => // ignore
        case _: Comment => // ignore
        case c => throw new IllegalContentWhereEventExpected("Found " + c.getCType.toString)
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
