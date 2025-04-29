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

import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType
import org.apache.daffodil.api.infoset.{ InfosetInputter => JInfosetInputter }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.Accessor
import org.apache.daffodil.lib.util.CursorImplMixin
import org.apache.daffodil.lib.util.MStackOfAnyRef
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ErrorERD
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

class InfosetError(kind: String, args: String*)
  extends ProcessingError("Infoset", Nope, Nope, kind, args: _*)

/**
 * Base class for objects that create Unparser infoset events from
 * some representation/source of infoset information like
 * XML objects, DOM trees, or XML text, or JSON text, etc.
 *
 * Events are pulled from this object via the usual iterator
 * hasNext()/next() idiom.
 *
 * However, next() does not return an object, but rather populates
 * an accessor by side effect, so this is a Cursor-style API.
 * You advance the cursor by calling next(), and the data appears under
 * the accessor.
 *
 * Supports a peek-style of operation by having two ways of advancing
 * and two accessors.
 *
 * Method advance is normal, and the advanceAccessor then has the data
 * of the event.
 *
 * Method inspect is a "peek", and the inspectAccessor then has the data
 * of the event, but the same data will be provided via the advanceAccessor
 * after the next advance call.
 *
 * Besides pulling events via inspect or advance,
 * the caller is responsible to maintain the TRD (TermRuntimeData) stack by
 * calling pushTRD and popTRD for all Terms (model-groups, and elements).
 * This is necessary so that the infoset inputter can resolve element
 * name + namespace into the proper ERD based on the proper dynamic context.
 */
final class InfosetInputter(actualInputter: JInfosetInputter)
  extends CursorImplMixin[InfosetAccessor]
  with NextElementResolver {

  type ERD = ElementRuntimeData

  private var isInitialized_ : Boolean = false

  def isInitialized = isInitialized_

  // Should not need to be initialized for performance reasons, this will be
  // set to an already allocated tunable when initialize() is called
  var tunable: DaffodilTunables = _

  final override lazy val advanceAccessor = InfosetAccessor()
  final override lazy val inspectAccessor = InfosetAccessor()

  private var documentElement_ : DIDocument = _

  /**
   * Mostly this is for unit testing where we want to inspect
   * the infoset the unparser constructed, as well as looking at
   * the data that was output.
   */
  def documentElement = {
    Assert.usage(isInitialized, "Must be initialized")
    documentElement_
  }

  /**
   * Initialize the infoset inputter, given the ERD of the root element, and
   * the tunables.
   */
  def initialize(rootElementInfo: ElementRuntimeData, tunableArg: DaffodilTunables): Unit = {
    tunable = tunableArg
    isInitialized_ = true

    documentElement_ = new DIDocument(rootElementInfo)
    infoStack.push(documentElement_)

    try {
      if (
        !actualInputter.hasNext || actualInputter.getEventType != InfosetInputterEventType.StartDocument
      ) {
        UnparseError(
          One(infoStack.top().erd.schemaFileLocation),
          Nope,
          "Infoset does not start with StartDocument event"
        )
      }
    } catch {
      case e: InvalidInfosetException =>
        UnparseError(
          One(infoStack.top().erd.schemaFileLocation),
          Nope,
          "Infoset does not start with StartDocument event: " + e.getMessage
        )
    }

    this.pushTRD(rootElementInfo) // TRD stack holds TRDs of model groups
  }

  private val infoStack = new Info.InfoStack

  private def level = infoStack.length

  private def indent = ("  " * level) + "|"

  override def toString() = {
    indent + "************* STATE ************************\n" +
      indent + "infoStack = " + infoStack + "\n" +
      indent + "********************************************"
  }

  /**
   * Queues used to hold additional events, thereby allowing a single pull to result
   * in multiple infoset events. For each pull, the first event coming back is not
   * queued, it is placed directly into the accessor for that type of pull.
   * Only additional extra events are queued.
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
  private val pendingInfos = new Array[Info](MaxPendingQueueSize)
  private val pendingStartOrEnd = new Array[InfosetEventKind](MaxPendingQueueSize)
  private var pendingCurIndex: Int = 0
  private var pendingLength: Int = 0

  private def queueElementStart(element: DIElement) =
    queueEventInfo_(InfosetEventKind.StartElement, Info(element))
  private def queueElementEnd(element: DIElement) =
    queueEventInfo_(InfosetEventKind.EndElement, Info(element))

  private def queueArrayStart(arrayERD: ERD) =
    queueEventInfo_(InfosetEventKind.StartArray, Info(arrayERD))

  private def queueEventInfo_(kind: InfosetEventKind, info: Info): Unit = {
    Assert.invariant(pendingLength < MaxPendingQueueSize)
    pendingInfos(pendingLength) = info
    pendingStartOrEnd(pendingLength) = kind
    pendingLength += 1
  }

  private def isPending: Boolean = pendingCurIndex < pendingLength

  /**
   * Scala compiler seems to avoid tuple allocation for 2-tuples
   * that are directly deconstructed by the caller. So this shouldn't
   * cause allocation of objects.
   */
  private def dequeuePending(): (InfosetEventKind, Info) = {
    Assert.invariant(pendingCurIndex >= 0)
    Assert.invariant(pendingCurIndex < pendingLength)
    Assert.usage(isPending)
    val p = (pendingStartOrEnd(pendingCurIndex), pendingInfos(pendingCurIndex))
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
      import InfosetEventKind._
      kind match {
        case StartElement => startElement(n.element)
        case EndElement => endElement(n.element)
        case StartArray => startArray(n.arrayERD)
        case EndArray => endArray(n.arrayERD)
      }
      true
    } else {
      try {
        reallyFill(advanceInput)
      } catch {
        case ex: InvalidInfosetException => {
          UnparseError(One(infoStack.top().erd.schemaFileLocation), Nope, ex.getMessage())
        }
      }
    }
  }

  private def reallyFill(advanceInput: Boolean): Boolean = {
    if (advanceInput) {
      actualInputter.next()
    }
    import InfosetInputterEventType._
    actualInputter.getEventType match {
      case StartElement => handleStartElement()
      case EndElement => handleEndElement()
      case StartDocument =>
        Assert.impossible() // should never happen due to the call to next() above
      case EndDocument => // ok. Just fall through
    }

    if (!actualInputter.hasNext) {
      Assert.invariant(actualInputter.getEventType == EndDocument)
      fini()
      false
    } else
      true
  }

  private def handleStartElement(): Unit = {
    val erd = nextElementErd()
    val node = createElement(erd)
    //
    // An ERD can come back that indicates an error.
    // We still want to create an infoset event, so that the consuming
    // unparsers can look at it to see if it is a start/end and possibly
    // correct the diagnostic from invalid element received, to
    // "expected an end, but got a start".
    //
    // Furthermore, an ERD can come back from inspect, where we're not
    // ready yet to attach to the infoset, because the ERD is for an
    // element that is later, and in the mean time we have to augment
    // the infoset with OutputValueCalc elements, or with defaultable
    // required elements that have no corresponding events in the
    // event stream.

    val top = infoStack.top()
    if (top.isComplexElement) {
      // An open complex element is on top of stack.
      // This start event must be for a child element
      val c = top.asComplex
      if (c.isNilled) {
        // cannot add content to a nilled complex element
        UnparseError(
          One(c.erd.schemaFileLocation),
          Nope,
          "Nilled complex element %s has content %s.",
          c.erd.namedQName.toExtendedSyntax,
          erd.namedQName.toExtendedSyntax
        )
      }
      if (erd.isArray) {
        // start of child which is an array
        startArray(erd)
        infoStack.push(erd)
        queueElementStart(node)
        infoStack.push(node)
      } else {
        Assert.invariant(!erd.isArray)
        // start of child which is a scalar element
        startElement(node)
        infoStack.push(node)
      }
    } else if (top.isSimpleElement) {
      // If a simple element is top of stack, we can't start another simple element
      // because that would be nesting them. We need an end-element first, which would pop the stack.
      UnparseError(
        One(top.erd.schemaFileLocation),
        Nope,
        "Simple type element %s cannot have children elements %s.",
        top.erd.namedQName.toExtendedSyntax,
        node.erd.namedQName.toExtendedSyntax
      )
    } else {
      // Top of stack indicates an array
      Assert.invariant(top.isArrayERD)
      // This start event must be for this array, or a subsequent element after the array
      if (erd.isArray) {
        // start event is for an array element
        if (top.erd eq erd) {
          // another peer occurrence in same array
          // leave the array on top of stack
          startElement(node)
          infoStack.push(node)
        } else {
          // end of current array, start of next array
          endArray(top.erd)
          val inv = infoStack.pop().isArrayERD
          Assert.invariant(inv)
          queueArrayStart(erd)
          infoStack.push(erd)
          queueElementStart(node)
          infoStack.push(node)
        }
      } else {
        // array being ended by a scalar element after it.
        Assert.invariant(!erd.isArray)
        Assert.invariant(top.erd ne erd) // can't be same element
        // end of current array, start of a scalar element
        endArray(top.erd)
        val inv = infoStack.pop().isArrayERD
        Assert.invariant(inv)
        queueElementStart(node)
        infoStack.push(node)
      }
    }
  }

  private def nextElementErd() =
    nextElement(
      actualInputter.getLocalName,
      actualInputter.getNamespaceURI,
      actualInputter.getSupportsNamespaces
    )

  private def createElement(erd: ERD) = {
    val elem = if (erd.isSimpleType) new DISimple(erd) else new DIComplex(erd)

    val optNilled = actualInputter.isNilled

    if (optNilled.isDefined) {
      if (!erd.isNillable) {
        UnparseError(
          One(elem.erd.schemaFileLocation),
          Nope,
          "Element %s defines nil property, but is not nillable",
          erd.namedQName.toExtendedSyntax
        )
      }
      if (optNilled.get) {
        elem.setNilled()
      }
    }

    if (erd.isSimpleType) {
      val txt =
        try {
          actualInputter.getSimpleText(erd.optPrimType.get, erd.runtimeProperties)
        } catch {
          case ex: NonTextFoundInSimpleContentException =>
            UnparseError(One(elem.erd.schemaFileLocation), Nope, ex.getMessage())
        }
      if (optNilled.isDefined && optNilled.get) {
        if (txt != null && txt != "") {
          UnparseError(
            One(elem.erd.schemaFileLocation),
            Nope,
            "Nilled simple element %s has content",
            erd.namedQName.toExtendedSyntax
          )
        }
      } else if (!erd.dpathElementCompileInfo.isOutputValueCalc) {
        val primType = elem.erd.optPrimType.get
        val obj =
          try {
            primType.fromXMLString(txt)
          } catch {
            case ipd: InvalidPrimitiveDataException =>
              UnparseError(One(elem.erd.schemaFileLocation), Nope, ipd.getMessage)
          }
        elem.asInstanceOf[DISimple].setDataValue(obj)
      }
    }

    elem
  }

  private def handleEndElement(): Unit = {
    val top = infoStack.top()

    if (top.isSimpleElement) {
      endElement(top.element)
      val inv = infoStack.pop().erd.isSimpleType
      Assert.invariant(inv)
      //
      // Can't check that end matches start because getLocalName() and getNamespaceURI() are
      // supposed to only be called during startElement(). Some/most infoset external representations
      // aren't like XML, and don't repeat name+namespace on end indications.
      //
    } else if (top.isComplexElement) {
      // This has to be end event for the current element
      endElement(top.element)
      val inv = infoStack.pop().erd.isComplexType
      Assert.invariant(inv)
    } else {
      // Top is an array
      Assert.invariant(top.isArrayERD)
      // This end event must be for the enclosing complex element
      val a = top.arrayERD
      endArray(a)
      val inv1 = infoStack.pop().isArrayERD
      Assert.invariant(inv1)
      Assert.invariant(infoStack.top().erd.isComplexType)
      val c = infoStack.top().asComplex
      queueElementEnd(c)
      infoStack.pop()
    }
  }

  /**
   * Fill current accessor with appropriate event information
   */
  private def startElement(node: DIElement): Unit = {
    accessor.kind = InfosetEventKind.StartElement
    accessor.info = Info(node)
  }

  /**
   * Fill current accessor with appropriate event information
   */
  private def endElement(node: DIElement): Unit = {
    accessor.kind = InfosetEventKind.EndElement
    accessor.info = Info(node)
  }

  /**
   * Fill current accessor with appropriate event information
   */
  private def startArray(arrayERD: ERD): Unit = {
    accessor.kind = InfosetEventKind.StartArray
    accessor.info = Info(arrayERD)
  }

  /**
   * Fill current accessor with appropriate event information
   */
  private def endArray(arrayERD: ERD): Unit = {
    accessor.kind = InfosetEventKind.EndArray
    accessor.info = Info(arrayERD)
  }

  override def fini(): Unit = actualInputter.fini();
}

// Performance Note - It's silly to use both a StartKind and EndKind accessor when
// delivering a simple type node. Separate start/end for XML makes sense because there
// are separate events for the contents between those tags, which can be widely separated.
// In Daffodil, the two events will always be back-to-back.
//
sealed trait InfosetEventKind {

  /**
   * Converts name like StartArray into "array start"
   *
   * Error messages depend on this word order.
   */
  override def toString() = {
    val cn = Misc.getNameFromClass(this)
    val initialLC = Misc.initialLowerCase(cn)
    val (se, ea) = initialLC.span(_.isLower)
    val res = ea.toLowerCase() + " " + se
    res
  }
}

/**
 * InfosetEvents are of these kinds.
 */
object InfosetEventKind {
  sealed trait StartKind extends InfosetEventKind
  sealed trait EndKind extends InfosetEventKind
  sealed trait ArrayKind extends InfosetEventKind
  sealed trait ElementKind extends InfosetEventKind
  case object StartElement extends StartKind with ElementKind
  case object EndElement extends EndKind with ElementKind
  case object StartArray extends StartKind with ArrayKind
  case object EndArray extends EndKind with ArrayKind
}

/**
 * An infoset event accessor.
 *
 * Not a case class because we don't want pattern matching on this. (It allocates to pattern match)
 *
 * The info associated with an infoset event is either a DIElement (for elements), or
 * an ElementRuntimeData for arrays. We don't want to use a DIArray object because those
 * are the private managed storage of DIComplex elements.
 *
 * At the time these info DIElements are created they are only to carry the infoset event information
 * they are not attached to parents/children in the infoset.
 *
 * We are effectively just using them as event "tuples" to carry the ERD and simple value
 * information after having resolved the incoming event's local name + namespace (if we're using those)
 * to get the ERD.
 *
 * By using our DIElement objects as the "tuples" of event information, we avoid having to
 * allocate yet another separate object to represent events.
 *
 * The Unparsers will examine the incoming DIElement, and attach child to parent if
 * they decide to keep them.
 *
 * Unparsers are also responsible for looking at the incoming event, and deciding if we
 * should first fill in any defaultable elements, or compute any dfdl:outputValueCalc elements.
 *
 * The advance/inspect change the state of the infoset inputter itself, but have no side-effects
 * on the Unparser's UState infoset.
 */
class InfosetAccessor private (var kind: InfosetEventKind, var info: Info)
  extends Accessor[InfosetAccessor] {

  def erd = info.erd

  def namedQName = erd.namedQName

  /**
   * This toString syntax is depended upon by diagnostic
   * messages, and many tests verify that.
   */
  override def toString = {
    val evLbl = if (kind eq null) "NullKind" else kind.toString
    val erdString = erd match {
      case errERD: ErrorERD => "(invalid) " + erd.namedQName.toExtendedSyntax
      case _ => erd.namedQName.toExtendedSyntax
    }
    evLbl + " event for " + erdString
  }

  /*
   * Methods to use instead of pattern matching
   */
  def isStart = kind.isInstanceOf[InfosetEventKind.StartKind]
  def isEnd = kind.isInstanceOf[InfosetEventKind.EndKind]
  def isElement = info.isElement
  def isArray = erd.isArray

  override def cpy() = new InfosetAccessor(kind, info)
  override def assignFrom(other: InfosetAccessor): Unit = {
    kind = other.kind
    info = other.info
  }
}

object InfosetAccessor {

  /**
   * Create an infoset accessor initialized to an element.
   */
  def apply(kind: InfosetEventKind.ElementKind, element: DIElement) =
    new InfosetAccessor(kind, Info(element))

  /**
   * Create an infoset accessor initialized to an array ERD.
   */
  def apply(kind: InfosetEventKind.ArrayKind, arrayERD: ElementRuntimeData) =
    new InfosetAccessor(kind, Info(arrayERD))

  /**
   * Construct an infoset accessor that is yet to be filled in.
   */
  def apply() =
    new InfosetAccessor(null, Info())
}

/**
 * AnyVal class that acts like a union type of DIElement and ERD.
 *
 * For arrays we just store an ERD. For elements we store the DIElement node
 * as a way of capturing the event information.
 */
final class Info private (val v: AnyRef) extends AnyVal with Serializable {
  def node = element
  def element = {
    Assert.usage(v ne null)
    v.asInstanceOf[DIElement]
  }
  def isComplexElement = isElement && element.isInstanceOf[DIComplex]
  def isSimpleElement = isElement && !isComplexElement
  def asComplex = element.asInstanceOf[DIComplex]
  def asSimple = element.asInstanceOf[DISimple]

  def isNode = isElement
  def isElement = {
    Assert.usage(v ne null)
    v.isInstanceOf[DIElement]
  }
  def arrayERD = {
    Assert.usage(v ne null)
    v.asInstanceOf[ElementRuntimeData]
  }
  def isArrayERD = {
    Assert.usage(v ne null)
    v.isInstanceOf[ElementRuntimeData]
  }
  def erd = v match {
    case null => Assert.usageError("v ne null")
    case arrayERD: ElementRuntimeData => arrayERD
    case diElement: DIElement => diElement.erd
  }

}

object Info {

  /**
   * Strongly typed stack that uses an AnyRef underneath.
   *
   * You can only store DIElement or ERD on this stack.
   */
  class InfoStack {
    private val delegate = MStackOfAnyRef()

    def push(e: DIElement): Unit = delegate.push(e)

    def push(arrayERD: ElementRuntimeData): Unit = {
      Assert.usage(arrayERD.isArray)
      delegate.push(arrayERD)
    }

    def pop() = Info.fromAnyRef(delegate.pop())

    def top() = Info.fromAnyRef(delegate.top)

    def length = delegate.length

    override def toString() = delegate.toString()
  }

  /**
   * Creates an unusable Info. Used to initialize
   * unpopulated accessors.
   */
  def apply() = nullValue

  /**
   * Create an ArrayERD Info
   */
  def apply(arrayERD: ElementRuntimeData): Info = {
    Assert.usage(arrayERD.isArray)
    new Info(arrayERD)
  }

  /**
   * Create a DIElement Info
   */
  def apply(element: DIElement): Info = new Info(element)

  private def fromAnyRef(anyRef: AnyRef): Info = anyRef match {
    case erd: ElementRuntimeData => Info(erd)
    case e: DIElement => Info(e)
    case _ => Assert.usageError("not called on ERD or DIElement")
  }

  private val nullValue = new Info(null)
}
