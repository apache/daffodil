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

package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import edu.illinois.ncsa.daffodil.util.Cursor
import edu.illinois.ncsa.daffodil.util.Accessor
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.CursorImplMixin
import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.api.DaffodilTunables


class InfosetError(kind: String, args: String*) extends ProcessingError("Infoset", Nope, Nope, kind, args: _*)

sealed trait InfosetInputterEventType
case object StartDocument extends InfosetInputterEventType
case object EndDocument extends InfosetInputterEventType
case object StartElement extends InfosetInputterEventType
case object EndElement extends InfosetInputterEventType

trait InfosetInputterCursor extends Cursor[InfosetAccessor] {
  /**
   * Override these further only if you want to say, delegate from one cursor implemtation
   * to another one.
   */
  override lazy val advanceAccessor = InfosetAccessor(null, null)
  override lazy val inspectAccessor = InfosetAccessor(null, null)

  def initialize(rootElement: ElementRuntimeData, tunable: DaffodilTunables)
}

abstract class InfosetInputter
  extends InfosetInputterCursor
  with CursorImplMixin[InfosetAccessor] {
  
  var tunable = DaffodilTunables()
  
  /**
   * Return the current infoset inputter event type
   */
  def getEventType(): InfosetInputterEventType 

  /**
   * Get the local name of the current event. This will only be called when the
   * current event type is StartElement.
   */
  def getLocalName(): String

  /**
   * Get the namespace of the current event. This will only be called when the
   * current event type is StartElement. If the InfosetInputter does not
   * support namespaces, this shoud return null. This may return null to
   * represent no namespaces.
   */
  def getNamespaceURI(): String

  /**
   * Get the content of a simple type. This will only be called when the
   * current event type is StartElement and the element is a simple type. If
   * the event contains complex data, it is an error and should throw
   * NonTextFoundInSimpleContentException. If the element does not have any
   * simple content, this should return either null or the empty string.
   */
  def getSimpleText(primType: NodeInfo.Kind): String

  /**
   * Determine if the current event is nilled. This will only be called when
   * the current event type is StartElement. Return MaybeBoolean.Nope if no
   * nil property is set, which implies the element is not nilled. Return
   * MaybeBoolean(false) if the nil property is set, but it is set to false.
   * Return MaybeBoolean(true) if the nil property is set to true.
   */
  def isNilled(): MaybeBoolean

  /**
   * Return true if there are remaining events. False otherwise.
   */
  def hasNext(): Boolean

  /**
   * Move the internal state to the next event.
   */
  def next(): Unit

  /**
   * Set if this infoset inputter supports namespaces. The return value of
   * getNamespace will be ignore if this is false. Note that if this is false,
   * some infoset representations may fail to unparse if the schema depends on
   * namespace information to differentiate between elements.
   */
  val supportsNamespaces: Boolean

  def initialize(rootElementInfo: ElementRuntimeData, tunableArg: DaffodilTunables): Unit = {
    tunable = tunableArg
    nextElementResolver =
      new OnlyOnePossibilityForNextElement(rootElementInfo.schemaFileLocation, rootElementInfo, RootResolver) // bootstrap

    val diDoc = new DIDocument(rootElementInfo, tunable)
    nodeStack.push(diDoc)

    try {
      if (!hasNext() || getEventType() != StartDocument) {
        UnparseError(One(nodeStack.top.erd.schemaFileLocation), Nope, "Infoset does not start with StartDocument event")
      }
    } catch {
      case e: InvalidInfosetException =>
        UnparseError(One(nodeStack.top.erd.schemaFileLocation), Nope, "Infoset does not start with StartDocument event: " + e.getMessage)
    }
  }

  private type NodeStack = MStackOf[DINode]
  private val nodeStack: NodeStack = new MStackOf[DINode]

  private var nextElementResolver: NextElementResolver = null

  private def level = nodeStack.length

  private def indent = ("  " * level) + "|"

  override def toString() = {
    indent + "************* STATE ************************\n" +
      indent + "nextElementResolver = " + nextElementResolver + "\n" +
      indent + "nodeStack = " + nodeStack + "\n" +
      indent + "********************************************"
  }

  /**
   * Queues used to hold additional events, thereby allowing a single pull to result
   * in multiple infoset events. For each pull, the first event coming back is not
   * queued. Only additional extra events are queued.
   *
   * Note that there should never be more than 3 events in the queue. This
   * occurs when ending an array following by a new array of simple elements.
   * In this case, we get events 1) End Old DIArray 2) Start New DIArray 3)
   * Start DISimple 4) End DISimple. The first event is stored in the cursor,
   * the remaining events are queued. We also always empty the queue before
   * enqueuing anything again. So implement this queue as an array of size 3,
   * and when all items have been dequeued, reset the indices to start the next
   * enqueues at the beginning of the array.
   */
  final private val MaxPendingQueueSize = 2
  private val pendingNodes = new Array[DINode](MaxPendingQueueSize)
  private val pendingStartOrEnd = new Array[InfosetEventKind](MaxPendingQueueSize)
  private var pendingCurIndex: Int = 0
  private var pendingLength: Int = 0

  private def queueAnotherEvent(kind: InfosetEventKind, node: DINode) {
    Assert.invariant(pendingLength < MaxPendingQueueSize)
    pendingNodes(pendingLength) = node
    pendingStartOrEnd(pendingLength) = kind
    pendingLength += 1
    if (kind eq EndKind) {
      node match {
        case f: DIFinalizable => f.setFinal()
        case _ => // ok
      }
    }
  }

  private def isPending: Boolean = pendingCurIndex < pendingLength

  private def dequeuePending(): (InfosetEventKind, DINode) = {
    Assert.invariant(pendingCurIndex >= 0)
    Assert.invariant(pendingCurIndex < pendingLength)
    Assert.usage(isPending)
    val p = (pendingStartOrEnd(pendingCurIndex), pendingNodes(pendingCurIndex))
    pendingCurIndex += 1
    if (pendingCurIndex == pendingLength) {
      // the last element of the queue has been removed. The queue is now
      // empty, so reset the indices to start filling the array from the
      // beginning on the next enqueue
      pendingCurIndex = 0
      pendingLength = 0
    }
    p
  }

  /**
   * Return true if the accessor was filled with another
   * infoset event, or one was taken from the queued events.
   */
  protected final def fill(): Boolean = {
    if (isPending) {
      val (kind, n) = dequeuePending()
      kind match {
        case StartKind => start(n)
        case EndKind => end(n)
      }
      true
    } else {
      try {
        reallyFill()
      } catch {
        case ex: InvalidInfosetException => {
          UnparseError(One(nodeStack.top.erd.schemaFileLocation), Nope, ex.getMessage())
        }
      }
    }
  }

  private def reallyFill(): Boolean = {
    next()

    getEventType() match {
      case StartElement => handleStartElement()
      case EndElement => handleEndElement()
      case StartDocument => Assert.impossible() // should never happen due to the call to next() above
      case EndDocument => // ok. Just fall through
    }

    if (!hasNext()) {
      Assert.invariant(getEventType() == EndDocument)
      Assert.invariant(nodeStack.top.isInstanceOf[DIDocument])
      fini
      false
    } else
      true
  }


  private def handleStartElement() {
    val e = createElement()
    //
    // There's state changes to the parent (which is complex or array)
    // and to the nodeStack to be done, and we have to either
    // populate the accessor with this node, or populate based on
    // an array transition.
    //
    if (nodeStack.top.isInstanceOf[DIComplex]) {
      val c = nodeStack.top.asInstanceOf[DIComplex]
      val optNilled = c.maybeIsNilled
      if (optNilled.isDefined && optNilled.get) {
        // cannot add content to a nilled complex element
        UnparseError(One(c.erd.schemaFileLocation), Nope, "Nilled complex element %s has content", c.erd.namedQName.toExtendedSyntax)
      }
      if (!e.erd.isArray) {
        c.addChild(e)
        start(e)
      } else {
        val a = c.getChildArray(e.erd).asInstanceOf[DIArray]
        a.append(e)
        nodeStack.push(a)
        //
        // The first event is created (and passed back) via the start method
        // Subsequent events that also need to be generated are queued by
        // way of calling queueAnotherEvent.
        //
        // The queue is not used for the first event.
        //
        start(a)
        queueAnotherEvent(StartKind, e)
        e.setParent(a.parent)
      }
    } else {
      // top must be an array
      val a = nodeStack.top.asInstanceOf[DIArray]
      if (!e.erd.isArray) {
        //
        // not an element of this array so we have to end this
        // array, and then see what this element requires
        //
        end(a)
        nodeStack.pop
        // top of stack now has to be a DIComplex since it held an array
        val c = nodeStack.top.asInstanceOf[DIComplex]
        c.addChild(e)
        queueAnotherEvent(StartKind, e)
      } else {
        // could be an element of this array, or start of the next one
        if (a.erd eq e.erd) {
          // same array
          a.append(e)
          start(e)
          e.setParent(a.parent)
        } else {
          // start of a different array
          end(a)
          nodeStack.pop
          val c = nodeStack.top.asInstanceOf[DIComplex]
          val nextA = c.getChildArray(e.erd).asInstanceOf[DIArray]
          nodeStack.push(nextA)
          queueAnotherEvent(StartKind, nextA)
          nextA.append(e)
          queueAnotherEvent(StartKind, e)
          e.setParent(nextA.parent)
        }
      }
    }

    nodeStack.push(e)

    Assert.invariant(e.parent ne null)
    nextElementResolver =
      if (e.erd.isSimpleType)
        e.erd.nextElementResolver
      else
        e.erd.childElementResolver
  }

  private def createElement() = {
    val erd = nextElementResolver.nextElement(getLocalName(), getNamespaceURI(), supportsNamespaces)
    val elem = if (erd.isSimpleType) new DISimple(erd) else new DIComplex(erd, tunable)

    val optNilled = isNilled()

    if (optNilled.isDefined) {
      if (!erd.isNillable) {
        UnparseError(One(elem.erd.schemaFileLocation), Nope, "Element %s defines nil property, but is not nillable", erd.namedQName.toExtendedSyntax)
      }
      if (optNilled.get) {
        elem.setNilled()
      }
    }

    if (erd.isSimpleType) {
      val txt =
        try {
          getSimpleText(erd.optPrimType.get)
        } catch {
          case ex: NonTextFoundInSimpleContentException =>
            UnparseError(One(elem.erd.schemaFileLocation), Nope, ex.getMessage())
        }
      if (optNilled.isDefined && optNilled.get) {
        if (txt != null && txt != "") {
          UnparseError(One(elem.erd.schemaFileLocation), Nope, "Nilled simple element %s has content", erd.namedQName.toExtendedSyntax)
        }
      } else if (erd.outputValueCalcExpr.isEmpty) {
        val primType = elem.erd.optPrimType.get
        val obj = primType.fromXMLString(txt)
        elem.asInstanceOf[DISimple].setDataValue(obj)
      }
    }

    elem
  }

  private def handleEndElement() {
    nodeStack.top match {
      case e: DIElement => {
        end(e)
        nodeStack.pop
        nextElementResolver = e.erd.nextElementResolver
      }
      case a: DIArray => {
        end(a)
        nodeStack.pop
        val parent = nodeStack.top.asInstanceOf[DIComplex]
        queueAnotherEvent(EndKind, parent)
        nodeStack.pop
        nextElementResolver = parent.erd.nextElementResolver
      }
      case node =>
        Assert.invariantFailed("Unexpected end element: " + node)
    }
  }

  private def start(node: DINode) {
    accessor.kind = StartKind
    accessor.node = node
  }

  private def end(node: DINode) {
    accessor.kind = EndKind
    accessor.node = node
    node match {
      case f: DIFinalizable => f.setFinal()
      case _ => // ok
    }
  }

}

object NonUsableInfosetInputter extends InfosetInputterCursor {
  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))
  override lazy val advanceAccessor = doNotUse
  override lazy val inspectAccessor = doNotUse
  override def advance = doNotUse
  override def inspect = doNotUse
  override def fini = doNotUse
  override def initialize(rootElement: ElementRuntimeData, tunable: DaffodilTunables) = doNotUse
}

// Performance Note - It's silly to use both a StartKind and EndKind accessor when
// delivering a simple type node. Separate start/end for XML makes sense because there
// are separate events for the contents between those tags, which can be widely separated.
// In Daffodil, the two events will always be back-to-back.... except....
// NOTE: for large blob/clob objects there's a reason for a start/end event
// because we may want to open a stream to get the contents in between without
// allocating a string to hold it.
//
sealed trait InfosetEventKind
case object StartKind extends InfosetEventKind { override def toString = "start" }
case object EndKind extends InfosetEventKind { override def toString = "end" }

/**
 * An infoset event accessor.
 *
 * Not a case class because we don't want pattern matching on this. (It allocates to pattern match)
 */
class InfosetAccessor private (var kind: InfosetEventKind, var node: DINode) extends Accessor[InfosetAccessor] {
  def namedQName = node.namedQName
  def erd: ElementRuntimeData = node match {
    case a: DIArray => a.parent.runtimeData
    case e: DIElement => e.runtimeData
  }

  override def toString = {
    val evLbl = if (kind eq null) "NullKind" else kind.toString
    val nodeKind = node match {
      case _: DIArray => "array"
      case _: DIElement => "element"
    }
    nodeKind + " " + evLbl + " event for " + erd.namedQName
  }

  /*
   * Methods to use instead of pattern matching
   */
  def isStart = kind == StartKind
  def isEnd = kind == EndKind
  def isElement = node.isInstanceOf[DIElement]
  def asElement: DIElement = node.asInstanceOf[DIElement]
  def isArray = node.isInstanceOf[DIArray]
  def asArray: DIArray = node.asInstanceOf[DIArray]
  def isComplex = node.isInstanceOf[DIComplex]
  def asComplex: DIComplex = node.asInstanceOf[DIComplex]
  def isSimple = node.isInstanceOf[DISimple]
  def asSimple: DISimple = node.asInstanceOf[DISimple]

  override def cpy() = new InfosetAccessor(kind, node)
  override def assignFrom(other: InfosetAccessor) {
    kind = other.kind
    node = other.node
  }
}

object InfosetAccessor {
  def apply(kind: InfosetEventKind, node: DINode) = new InfosetAccessor(kind, node)
}
