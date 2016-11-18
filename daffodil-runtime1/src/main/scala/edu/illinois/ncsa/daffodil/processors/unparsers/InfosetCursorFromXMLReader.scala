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
import edu.illinois.ncsa.daffodil.util.CursorImplMixin
import edu.illinois.ncsa.daffodil.util.MStackOf
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamConstants._
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.util.XMLEventAllocator
import javax.xml.XMLConstants
import edu.illinois.ncsa.daffodil.processors.DIFinalizable
import javax.xml.stream.XMLStreamException

/**
 * Converts the XML representation of a Daffodil DFDL Infoset into an
 * actual Daffodil DFDL Infoset tree and provides a cursor-based in-order walk of the
 * tree nodes.
 *
 * Detaching/discarding Infoset tree nodes that are no longer needed is responsibility
 * of the caller.
 *
 * The primary goal for this converter is
 * "Streaming Behavior" meaning it doesn't need space proportional to how
 * big the input data is, in order to carry out the conversion. That is to say
 * the user of this converter can pull infoset events from it one at a time without fear
 * that the converter itself is using up ever more and more memory.
 *
 * The infoset events; however, are just indicators of an in-order traversal of the
 * infoset tree which is being constructed incrementally from the XML events,
 * so unless the code calling this prunes the infoset tree (removing infoset nodes it no longer
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
private[unparsers] final class InfosetCursorFromXMLReader(
  rdr: java.io.Reader,
  rootElementInfo: ElementRuntimeData)
  extends InfosetCursor
  with CursorImplMixin[InfosetAccessor] {

  private lazy val fact = {
    val fact = XMLInputFactory.newFactory()

    // JIRA DFDL-1659 - make sure not accessing things remotely and protect from denial-of-service
    // using XML trickery.
    // fact.setProperty("http://javax.xml.XMLConstants/property/accessExternalDTD", false)
    // fact.setProperty("http://xml.org/sax/features/external-general-entities", false)
    // fact.setProperty("http://xml.org/sax/features/external-parameter-entities", false)
    //
    // fact.setProperty(XMLConstants.FEATURE_SECURE_PROCESSING, true) // Seems to be the default setting anyway

    fact.setProperty(XMLInputFactory.IS_COALESCING, true)
    fact.setEventAllocator(new com.sun.xml.internal.stream.events.XMLEventAllocatorImpl())

    fact
  }

  override def fini {
    xsr.close()
  }

  /**
   * evAlloc is only to be used for diagnostic messages. It lets us easily
   * capture and toString the event information. But we don't call it otherwise
   * as it allocates an object, and we're trying to avoid that.
   */
  private lazy val (xsr: XMLStreamReader, evAlloc: XMLEventAllocator) = {
    val xsr = fact.createXMLStreamReader(rdr)
    //
    // This gets the event allocator corresponding to the xmlStreamReader just created.
    // Strange API. They should let you get this from the xmlStreamReader itself.
    //
    val evAlloc = fact.getEventAllocator
    Assert.invariant(
      // no need for UnparseError here. If the XML syntax is bad, parser catches it before we get here.
      xsr.hasNext())
    val evNum = xsr.getEventType()
    Assert.invariant(evNum == START_DOCUMENT)
    nodeStack.push(diDoc)
    (xsr, evAlloc)
  }

  private val initialNextElementResolver =
    new OnlyOnePossibilityForNextElement(rootElementInfo.schemaFileLocation, rootElementInfo, RootResolver) // bootstrap

  private val diDoc = new DIDocument(rootElementInfo)

  private type NodeStack = MStackOf[DINode]
  private val nodeStack: NodeStack = new MStackOf[DINode]

  private var nextElementResolver: NextElementResolver = initialNextElementResolver

  private def level = nodeStack.length

  private def indent = ("  " * level) + "|"

  override def toString() = {
    indent + "************* STATE ************************\n" +
      indent + "nextElementResolver = " + nextElementResolver + "\n" +
      indent + "nodeStack = " + nodeStack + "\n" +
      indent + "********************************************"
  }

  //  private def dumpState = {
  //    println(this)
  //  }

  private def isNilled(s: DIElement) = s.maybeIsNilled.isDefined && (s.maybeIsNilled.get == true)

  /**
   * Queues used to hold additional events, thereby allowing a single pull to result
   * in multiple infoset events. For each pull, the first event coming back is not
   * queued. Only additional extra events are queued.
   */
  private val pendingNodes = new scala.collection.mutable.Queue[DINode]
  private val pendingStartOrEnd = new scala.collection.mutable.Queue[InfosetEventKind]

  private def queueAnotherEvent(kind: InfosetEventKind, node: DINode) {
    pendingNodes.enqueue(node)
    pendingStartOrEnd.enqueue(kind)
    if (kind eq EndKind) {
      node match {
        case f: DIFinalizable => f.setFinal()
        case _ => // ok
      }
    }
  }

  private def isPending: Boolean = pendingNodes.length > 0

  private def dequeuePending(): (InfosetEventKind, DINode) = {
    Assert.usage(isPending)
    (pendingStartOrEnd.dequeue(), pendingNodes.dequeue())
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
      reallyFill()
    }
  }

  private def top = nodeStack.top

  private def reallyFill(): Boolean = {
    val xmlEvent = nextTagOrEndDocument()
    xmlEvent match {
      case START_ELEMENT =>
        handleStartElement()
      case END_ELEMENT =>
        handleEndElement()
      case END_DOCUMENT =>
      // ok. Just fall through
    }

    if (!xsr.hasNext()) {
      Assert.invariant(xsr.getEventType() == END_DOCUMENT)
      Assert.invariant(top == diDoc)
      xsr.close()
      false
    } else
      true
  }

  /**
   * Almost same as xsr.nextTag(), but returns normally on
   * END_DOCUMENT as well as START_ELEMENT and END_ELEMENT
   *
   * returns -1 if called after END_DOCUMENT.
   */
  private def nextTagOrEndDocument(): Int = {
    var result: Int = -1
    var exitNow = false
    while (xsr.hasNext() && !exitNow) {
      result =
        try {
          xsr.next()
        } catch {
          case xse: XMLStreamException => {
            InvalidInfosetXML.errorWhenTagExpected(top.erd, xse, evAlloc.allocate(xsr))
          }
        }
      result match {
        case START_ELEMENT | END_ELEMENT | END_DOCUMENT => exitNow = true
        case CHARACTERS if xsr.isWhiteSpace() => // skip whitespace
        case CDATA if xsr.isWhiteSpace() => // skip whitespace
        case SPACE | PROCESSING_INSTRUCTION | COMMENT => // skip these too
        case other =>
          InvalidInfosetXML.illegalContentWhereTagExpected(top.erd, evAlloc.allocate(xsr))
      }
    }
    result
  }

  private def handleStartElement() {
    val elem = createElement()
    //
    // There's state changes to the parent (which is complex or array)
    // and to the nodeStack to be done, and we have to either
    // populate the accessor with this node, or populate based on
    // an array transition.
    //
    (top, elem) match {
      case (c: DIComplex, e: DIElement) if (!e.erd.isArray) => {
        c.addChild(e)
        start(e)
      }
      case (c: DIComplex, e: DIElement) if (e.erd.isArray) => {
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
      case (a: DIArray, e: DIElement) if (!e.erd.isArray) => {
        //
        // not an element of this array so we have to end this
        // array, and then see what this element requires
        //
        end(a)
        nodeStack.pop
        // top of stack now has to be a DIComplex since it held an array
        val c = top.asInstanceOf[DIComplex]
        c.addChild(e)
        queueAnotherEvent(StartKind, e)
      }
      case (a: DIArray, e: DIElement) if (e.erd.isArray) => {
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
          val c = top.asInstanceOf[DIComplex]
          val nextA = c.getChildArray(e.erd).asInstanceOf[DIArray]
          nodeStack.push(nextA)
          queueAnotherEvent(StartKind, nextA)
          nextA.append(e)
          queueAnotherEvent(StartKind, e)
          e.setParent(nextA.parent)
        }
      }
      case other => Assert.invariantFailed("Cannot be " + other)
    }
    elem match {
      case c: DIComplex => nodeStack.push(elem)
      case s: DISimple => queueAnotherEvent(EndKind, elem)
    }
    Assert.invariant(elem.parent ne null)
    nextElementResolver =
      if (elem.erd.isSimpleType)
        elem.erd.nextElementResolver
      else
        elem.erd.childElementResolver
  }

  private def createElement() = {
    Assert.usage(xsr.getEventType() == START_ELEMENT)
    val erd = getStartERD()
    val elem = makeElement(erd)
    elem match {
      case s: DISimple => {
        val txt =
          try {
            xsr.getElementText()
          } catch {
            case xse: XMLStreamException => {
              InvalidInfosetXML.nonTextFoundInSimpleContent(erd, evAlloc.allocate(xsr))
            }
          }
        Assert.invariant(xsr.getEventType() == END_ELEMENT)
        if (!isNilled(elem) && s.erd.outputValueCalcExpr.isEmpty) {
          Assert.invariant(verifyName(s.erd))
          val primType = s.erd.optPrimType.get
          val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(txt)
          val obj = primType.fromXMLString(remapped)
          s.setDataValue(obj)
        }
      }
      case _ => // ok
    }
    elem
  }

  def handleEndElement() {
    top match {
      case c: DIComplex => {
        end(c)
        Assert.invariant(verifyName(c.erd))
        nodeStack.pop
        nextElementResolver = c.erd.nextElementResolver
      }
      case a: DIArray => {
        end(a)
        nodeStack.pop
        val parent = nodeStack.top.asInstanceOf[DIComplex]
        Assert.invariant(verifyName(parent.erd))
        queueAnotherEvent(EndKind, parent)
        nodeStack.pop
        nextElementResolver = parent.erd.nextElementResolver
      }
      case _ =>
        Assert.invariantFailed("Unexpected event: " + evAlloc.allocate(xsr))
    }
  }

  private def getStartERD() = {
    val ns = xsr.getNamespaceURI
    val local = xsr.getLocalName
    val thisERD = nextElementResolver.nextElement(local, ns)
    thisERD
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

  private def makeElement(erd: ElementRuntimeData) = {
    val newNode =
      if (erd.isSimpleType) {
        createNewSimpleElement(erd)
      } else {
        Assert.invariant(erd.isComplexType)
        createNewComplexElement(erd)
      }
    newNode
  }

  private def createNewSimpleElement(erd: ElementRuntimeData): DISimple = {
    Assert.invariant(xsr.getEventType() == START_ELEMENT)
    val newNode = new DISimple(erd)
    if (erd.isNillable && isNilXML()) {
      newNode.setNilled()
    }
    newNode
  }

  private def isNilXML(): Boolean = {
    Assert.invariant(xsr.getEventType() == START_ELEMENT)
    val nAttrs = xsr.getAttributeCount()
    // There are attributes. Look for xsi:nil. Ignore all other attributes
    var i = 0
    var exitNow = false
    var res: Boolean = false
    while (i < nAttrs && !exitNow) {
      val attrName = xsr.getAttributeLocalName(i)
      if (attrName == "nil") {
        // attributes local name is "nil"
        // Most likely this is xsi:nil="true"
        val attrNS = xsr.getAttributeNamespace(i)
        val attrValue = xsr.getAttributeValue(i)
        if (attrValue == "true" &&
          attrNS == XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI)
          res = true
        exitNow = true
      }
      i += 1
    }
    res
  }

  private def createNewComplexElement(erd: ElementRuntimeData): DIComplex = {
    Assert.invariant(xsr.getEventType() == START_ELEMENT)
    val newNode = new DIComplex(erd)
    if (isNilXML()) {
      // complex element but Nilled, which is same as a simple type but nilled.
      newNode.setNilled()
    }
    newNode
  }

  private def verifyName(erd: ElementRuntimeData): Boolean = {
    val nqn = erd.namedQName
    val erdNS = nqn.namespace.toStringOrNullIfNoNS
    val erdLocal = nqn.local
    val ns = NS(xsr.getNamespaceURI).toStringOrNullIfNoNS
    val local = xsr.getLocalName
    val res = (erdNS =:= ns) &&
      (erdLocal =:= local)
    res
  }

}

