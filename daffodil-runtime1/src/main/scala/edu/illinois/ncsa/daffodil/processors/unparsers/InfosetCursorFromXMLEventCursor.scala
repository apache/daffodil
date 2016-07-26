/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIDocument
import scala.annotation.tailrec
import edu.illinois.ncsa.daffodil.util.CursorImplMixin
import XMLEvent._
import edu.illinois.ncsa.daffodil.util.InvertControl
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.MStackOf

/**
 * The primary goal for this converter is
 * "Streaming Behavior" meaning it doesn't need space proportional to how
 * big the input data is, in order to carry out the conversion. That is to say
 * the user of this converter can pull infoset events from it one at a time without fear
 * that the converter itself is using up ever more and more memory.
 *
 * The infoset events; however, are just indicators of an in-order traversal of the
 * infoset tree which is being constructed incrementally from the XML events,
 * so unless the code consuming
 * events from this prunes the infoset tree (removing infoset nodes it no longer
 * needs), then the infoset tree will grow without bound.
 *
 * That however, is not the responsibility of this converter, but of the
 * consuming activity.
 *
 * Stateful, not thread safe.
 *
 * This object has state. Therefore it cannot be shared across threads. An unparser
 * call must construct its own instance of this object for the thread running that
 * unparse method call.
 */
private[unparsers] class InfosetCursorFromXMLEventCursor(xmlCursor: XMLEventCursor, rootElementInfo: ElementRuntimeData)
    extends InfosetCursor
    with CursorImplMixin[InfosetAccessor]
    with InvertControl[InfosetAccessor] {

  private def iter = this.asInstanceOf[InvertControl[InfosetAccessor]]

  /**
   * Fills current accessor with next infoset event.
   */
  override protected def fill: Boolean = {
    if (iter.hasNext) {
      iter.next()
      true
    } else false
  }

  private val initialNextElementResolver =
    new OnlyOnePossibilityForNextElement(rootElementInfo.schemaFileLocation, rootElementInfo, RootResolver) // bootstrap

  private val diDoc = new DIDocument(rootElementInfo)

  private type NodeStack = MStackOf[DINode]
  private val nodeStack: NodeStack = new MStackOf[DINode]

  private var nextElementResolver: NextElementResolver = initialNextElementResolver

  private var level = 0

  private def indent = ("  " * level) + "|"

  override def toString() = {
    indent + "************* STATE ************************\n" +
      indent + "nextElementResolver = " + nextElementResolver + "\n" +
      indent + "sb = '" + sb.toString() + "'\n" +
      indent + "nodeStack = " + nodeStack + "\n" +
      indent + "infoset = " + diDoc + "\n" +
      indent + "********************************************"
  }

  //  private def dumpState = {
  //    println(this)
  //  }

  def body {
    try {
      nodeStack.push(diDoc)
      recursivelyCreateTreeFromXMLEvents(nodeStack)
      Assert.invariant(nodeStack.length == 1)
      Assert.invariant(nodeStack.top == diDoc)
    } catch {
      case rsde: RuntimeSchemaDefinitionError => iter.setFinal(rsde)
      case pe: ProcessingError => iter.setFinal(pe)
    } finally {
      xmlCursor.fini // we finished, tell the xmlCursor to finish too
    }
  }

  @tailrec
  private def recursivelyCreateTreeFromXMLEvents(parents: NodeStack) {
    if (!xmlCursor.advance) return
    val xmlEvent = xmlCursor.advanceAccessor.event
    xmlEvent match {
      case evStart: EvStart => {
        Assert.invariant(xmlEvent ne null)
        //        println(indent + "starting event: " + xmlEvent)
        //        dumpState
        level += 1
        handleEvStart(evStart, parents)
      }
      case evEnd: EvEnd => {
        val ended = handleEvEnd(evEnd, parents)
        level -= 1
        //        println(indent + "ending event: " + xmlEvent)
        //        dumpState
        ended match {
          case e: DIElement if (e.erd =:= rootElementInfo) => return // just ended the root
          case s: DISimple if (s.erd.outputValueCalcExpr.isDefined) =>
          // Doesn't hold. We attempt the OVC calc. If it works we're good, otherwise we suspend it.
          // Assert.invariant(!s.hasValue && !s.isNilled) // OVC DISimple's are reset so they should have no value
          case s: DISimple => Assert.invariant(s.hasValue || s.isNilled)
          case c: DIComplex => Assert.invariant((c.totalElementCount > 0) || c.isNilled)
          case _: DIArray => Assert.impossible() // DIArray should not have ended without also ending something else afterwards in handleEvEnd
        }
      }
      case evText: EvText => {
        handleEvText(evText, parents)
      }
      case er: EvEntityRef => {
        handleEvEntityRef(er, parents)
      }
      case _ => Assert.invariantFailed("That event type " + xmlEvent + " should not be present.")
    }

    recursivelyCreateTreeFromXMLEvents(parents)
  }

  /**
   * This is where the simple type string content builds up.
   * On an EvEnd of a simple type element, we will grab this
   * and convert to the value type.
   */
  private val sb = new StringBuilder

  private def handleEvText(evText: EvText, parents: NodeStack) {
    handleSimpleTextualEvent(evText, evText.text, parents)
  }

  private def handleEvEntityRef(er: EvEntityRef, parents: NodeStack) {
    val entity = er.entity
    val text = decodeEntityRef(entity)
    handleSimpleTextualEvent(er, text, parents)
  }

  private def handleSimpleTextualEvent(ev: XMLEvent, text: String, parents: NodeStack) {
    parents.top match {
      case diSimple: DISimple => {
        if (diSimple.isNilled) Assert.invariantFailed("text content found in nilled element")
        else sb.append(text)
      }
      case _ if text.matches("\\s*") => // ok. Absorb whitespace between elements
      case e: DIElement =>
        InvalidInfosetXML.textInElementOnlyContent(e.erd, xmlCursor, ev)
      case a: DIArray =>
        InvalidInfosetXML.textInElementOnlyContent(a.erd, xmlCursor, ev)
    }
  }

  private def decodeEntityRef(er: String): String = {
    er match {
      case "gt" => ">"
      case "lt" => "<"
      case "amp" => "&"
      case "apos" => "'"
      case "quot" => "\""
      case _ => Assert.usageError("Not an accepted entity ref")
    }
  }

  private def getStartERD(eev: XMLElementEvent) = {
    val ns = eev.getNamespaceStringOrNullIfNoNS
    val local = eev.label
    val thisERD = nextElementResolver.nextElement(local, ns)
    thisERD
  }

  private def handleEvStart(evStart: EvStart, parents: NodeStack) {
    val erd = getStartERD(evStart)
    parents.top match {
      // already an array node
      case a: DIArray => {
        handleEventDuringArray(erd, a, evStart, parents)
      }
      // parent is not an array. But child will be. Create new array & recurse
      case c: DIComplex if erd.isArray => {
        val a = c.getChildArray(erd).asInstanceOf[DIArray] // creates if it doesn't exist (which it shouldn't)
        parents.push(a)
        start(a)
        // recursively handle it. Now it WILL have an array on top of parents
        handleEvStart(evStart, parents)
      }
      // parent is not an array, child is non-array
      case c: DIComplex => {
        val e = makeElement(erd, evStart)
        c.addChild(e)
        if (e.erd.isComplexType) {
          // don't do this for simple elements until we see the end. Because we don't have a value until then
          // and we don't want to hand back unpopulated elements to the consumer.
          start(e)
        }
        parents.push(e)
        nextElementResolver = if (erd.isSimpleType) e.erd.nextElementResolver else e.erd.childElementResolver
      }
      case s: DISimple => // parent is simple, but we got an EvStart
        InvalidInfosetXML.elementFoundInSimpleContent(s.erd, erd, xmlCursor, evStart)
      case _ => Assert.impossibleCase()
    }
  }

  private def handleEventDuringArray(erd: ElementRuntimeData, arr: DIArray, ev: XMLEvent, parents: NodeStack) {
    ev match {
      // Another element of same array
      case evStart: EvStart if erd.isArray && (arr.erd _eq_ erd) => {
        val e = makeElement(erd, evStart)
        arr.parent.addChild(e) // we go up to the parent of the array, and do this add, which navigates back down to the array location.
        if (e.erd.isComplexType) start(e)
        parents.push(e)
        nextElementResolver = if (erd.isSimpleType) erd.nextElementResolver else erd.childElementResolver
      }
      // incoming occurrence for an array, but not same array
      case evStart: EvStart if erd.isArray && (arr.erd _ne_ erd) => {
        end(arr)
        arr.isFinal = true
        parents.pop
        handleEvStart(evStart, parents) // recursively
      }
      // incoming occurrence not an array at all
      case evStart: EvStart if !erd.isArray => {
        end(arr)
        arr.isFinal = true
        parents.pop
        handleEvStart(evStart, parents)
      }
      // incoming event is end of the enclosing parent of the array
      case evEnd: EvEnd if (arr.erd _eq_ erd) => {
        Assert.invariant(erd eq arr.parent.erd)
        end(arr)
        arr.isFinal = true
        parents.pop
        handleEvEnd(evEnd, parents)
      }
      case _ => Assert.invariantFailed("Unexpected event: " + ev + " for array " + arr)
      //
    }
  }

  private def start(node: DINode) {
    accessor.kind = StartKind
    accessor.node = node
    iter.setNext(accessor)
  }

  private def end(node: DINode) {
    accessor.kind = EndKind
    accessor.node = node
    iter.setNext(accessor)
  }

  private def makeElement(erd: ElementRuntimeData, ev: EvStart) = {
    val newNode =
      if (erd.isSimpleType) {
        createNewSimpleElement(ev, erd)
      } else {
        Assert.invariant(erd.isComplexType)
        createNewComplexElement(ev, erd)
      }
    newNode
  }

  private def createNewSimpleElement(evStart: EvStart, erd: ElementRuntimeData): DISimple = {
    val newNode = new DISimple(erd)
    if (erd.isNillable && evStart.isNil) {
      newNode.setNilled()
    }
    newNode
  }

  private def createNewComplexElement(evStart: EvStart, erd: ElementRuntimeData): DIComplex = {
    val newNode = new DIComplex(erd)
    if (evStart.isNil) {
      // complex element but Nilled, which is same as a simple type but nilled.
      newNode.setNilled()
    }
    newNode
  }

  private def handleEvEnd(evEnd: EvEnd, parents: NodeStack) = {
    //
    // an important invariant, is that when recursing over an element, we push the node onto the
    // parents node stack while recursing over the children. So the parent during the
    // processing of content of a simple type is always the simple type node.
    //
    // the parent when recursing over the children of a complex type is either the
    // complex type node, or an array child thereof.
    //
    // When we get an EvEnd for an element, if it's simple, then the
    // parents.top is the node.
    parents.top match {
      case s: DISimple => {
        Assert.invariant(verifyName(evEnd, s.erd))
        if (!s.isNilled) {
          val txt = sb.toString
          sb.clear()
          val primType = s.erd.optPrimType.get
          val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(txt)
          val obj = primType.fromXMLString(remapped)
          s.setDataValue(obj)
          nextElementResolver = s.erd.nextElementResolver // to resolve next sibling
        }
        start(s)
        end(s)
      }
      case c: DIComplex => {
        //
        // same is true for a EvEnd for a complex type. The parents.top
        // will be the complex node itself.
        //
        Assert.invariant(verifyName(evEnd, c.erd))
        end(c)
        c.isFinal = true
        nextElementResolver = c.erd.nextElementResolver // to resolve next sibling after this complex element
      }
      case a: DIArray => {
        // must distinguish two cases here
        //
        // one is this is the end of a element occurrence in the array (one of the array's elements)
        // That cannot happen. The parent in that case would be the simple type or complex type that
        // this EvEnd is ending.
        //
        // the other is this is the end of the enclosing complex type surrounding the array.
        // In that case we want to end the array first, then the complex element.
        Assert.invariant(verifyName(evEnd, a.parent.erd))
        end(a)
        a.isFinal = true
        parents.pop
        val c = a.parent
        end(c)
        c.isFinal = true
        nextElementResolver = c.erd.nextElementResolver // to resolve next sibling after this complex element
      }
    }
    parents.pop
  }

  private def verifyName(evEnd: EvEnd, erd: ElementRuntimeData): Boolean = {
    val nqn = erd.namedQName
    val erdNS = nqn.namespace.toStringOrNullIfNoNS
    val erdLocal = nqn.local
    val ns = evEnd.getNamespaceStringOrNullIfNoNS
    val local = evEnd.label
    val res = (erdNS =:= ns) &&
      (erdLocal =:= local)
    res
  }

}
