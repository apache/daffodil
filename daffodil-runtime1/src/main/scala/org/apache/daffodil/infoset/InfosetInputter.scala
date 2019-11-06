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

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.ProcessingError
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.util.Cursor
import org.apache.daffodil.util.Accessor
import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.CursorImplMixin
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.processors.ElementRuntimeData

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
  override lazy val advanceAccessor = InfosetAccessor(null, null, null)
  override lazy val inspectAccessor = InfosetAccessor(null, null, null)

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

  override def inspectPure: Boolean = {
    accessor = inspectAccessor

    val res = priorOpKind match {
      case Advance => {
        priorOpKind = InspectPure
        if (isPending) {
          accessor.kind = pendingStartOrEnd(pendingCurIndex)
          accessor.node = pendingNodes(pendingCurIndex)
          accessor.erd = accessor.node.erd
        } else {
          next() //advance the input stream so nextElementErd is pointing to the correct place
          val eventKind = getEventType() match {
            case StartElement => StartKind
            case EndElement => EndKind
            case StartDocument => StartKind
            case EndDocument => EndKind
          }
          accessor.kind = eventKind
          if (eventKind == StartKind) {
            accessor.erd = nextElementErd()
            accessor.node = null
          } else {
            //accessor.kind == EndKind
            val node = nodeStack.top
            accessor.node = node
            accessor.erd = node.erd
          }
        }
        true
      }
      case InspectPure => true
      case Inspect => true
      case Unsuccessful => false
    }

    res
  }

  /**
   * Queues used to hold additional events, thereby allowing a single pull to result
   * in multiple infoset events. For each pull, the first event coming back is not
   * queued. Only additional extra events are queued.
   *
   * Note that there should never be more than 2 events in the queue. This
   * occurs when ending an array following by a new array of simple elements.
   * In this case, we get events 1) End Old DIArray 2) Start New DIArray 3)
   * Start new element. The first event is stored in the cursor,
   * the remaining events are queued. We also always empty the queue before
   * enqueuing anything again. So implement this queue as an array of size 2,
   * and when all items have been dequeued, reset the indices to start the next
   * enqueues at the beginning of the array.
   *
   * To avoid tuple allocation and overhead, this is done as a pair of arrays, not
   * an array of pairs.
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
  protected final def fill(advanceInput: Boolean): Boolean = {
    if (isPending) {
      val (kind, n) = dequeuePending()
      kind match {
        case StartKind => start(n)
        case EndKind => end(n)
      }
      true
    } else {
      try {
        reallyFill(advanceInput)
      } catch {
        case ex: InvalidInfosetException => {
          UnparseError(One(nodeStack.top.erd.schemaFileLocation), Nope, ex.getMessage())
        }
      }
    }
  }

  private def reallyFill(advanceInput: Boolean): Boolean = {
    if (advanceInput) {
      next()
    }

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
        c.addChild(e) // addChild here sets the 'array' value
        val a = e.array.get.asInstanceOf[DIArray]
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
          c.addChild(e) // addChild here sets the 'array' value
          val nextA = e.array.get.asInstanceOf[DIArray]
          nodeStack.push(nextA)
          queueAnotherEvent(StartKind, nextA)
          queueAnotherEvent(StartKind, e)
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

  private def nextElementErd() = nextElementResolver.nextElement(getLocalName(), getNamespaceURI(), supportsNamespaces)

  private def createElement() = {
    val erd = nextElementErd()
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
    accessor.erd = node.erd
  }

  private def end(node: DINode) {
    accessor.kind = EndKind
    accessor.node = node
    accessor.erd = node.erd
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
  override def inspectPure = doNotUse
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
 *
 * We have erd as a seperate field (instead of just using node.erd) since
 * the inspectPure method finds an erd without constructing the corresponding DINode (so as to avoid side effects)
 *
 * We maintaint the invariant that node==null || node.erd==erd
 *
 */
class InfosetAccessor private (var kind: InfosetEventKind, var node: DINode, var erd: ElementRuntimeData) extends Accessor[InfosetAccessor] {
  def namedQName = erd.namedQName

  Assert.invariant(node == null || node.erd == erd)

  override def toString = {
    val evLbl = if (kind eq null) "NullKind" else kind.toString
    val nodeKind = node match {
      case null => "?"
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

  override def cpy() = new InfosetAccessor(kind, node, erd)
  override def assignFrom(other: InfosetAccessor) {
    kind = other.kind
    node = other.node
    erd = other.erd
  }
}

object InfosetAccessor {
  def apply(kind: InfosetEventKind, node: DINode, erd: ElementRuntimeData) = new InfosetAccessor(kind, node, erd)
}
