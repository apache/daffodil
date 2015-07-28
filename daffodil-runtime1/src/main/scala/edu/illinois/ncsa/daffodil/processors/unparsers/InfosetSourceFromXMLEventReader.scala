package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIDocument
import scala.annotation.tailrec
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.OnStack

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
class InfosetSourceFromXMLEventReader(
  xer: Iterator[scala.xml.pull.XMLEvent],
  rootElementInfo: ElementRuntimeData) extends InfosetSource {

  private lazy val xmlIterator = new XMLEventIterator(xer)

  private val initialNextElementResolver =
    new OnlyOnePossibilityForNextElement(rootElementInfo.schemaFileLocation, rootElementInfo, RootResolver) // bootstrap

  private lazy val diDoc = new DIDocument(rootElementInfo)

  private val nodeStack = new MStack.Of[DIComplex]
  nodeStack.push(diDoc)

  /**
   * Represents Maybe[DIArray] as just DIArray with null meaning Nope.
   * This avoids allocating a Maybe object for every push.
   *
   * (Seems to be a Scala bug that you cannot create an Iterator[Maybe[T]]. It fails
   * to compile the definition of next() : Maybe[T], so we have to go back to
   * the old null vs. regular object reference.
   */
  private val arrayStack = new MStack.OfMaybe[DIArray]
  arrayStack.pushMaybe(Nope)

  private var nextElementResolver: NextElementResolver = initialNextElementResolver

  private val savedEvents = new mutable.ArrayBuffer[InfosetEvent]

  private def fill {
    if (savedEvents.isEmpty) {
      nextEvents(savedEvents)
    }
  }

  override def peek = {
    fill
    savedEvents.head
  }

  override def next = {
    fill
    val thisEvent = savedEvents.head
    savedEvents.remove(0)
    thisEvent
  }

  /**
   * Fills array buffer of infoset events given the next xml event.
   *
   * This will include events for any implied elements such as
   * required defaultable elements, or elements with outputValueCalc.
   */
  private def nextEvents(accumulatedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = {
    Assert.usage(accumulatedEvents.isEmpty)
    val xmlEvent: XMLInfosetEvent = xmlIterator.next()
    xmlEvent match {
      case EndComplex(ns, local) => {
        val node = nodeStack.pop
        val arr = arrayStack.popMaybe
        if (arr.isDefined)
          accumulatedEvents += End(arr.get)
        nextElementResolver = node.runtimeData.nextElementResolver // FIXME: Not strict enough. This will accept children after the element.....
        accumulatedEvents.append(End(node))
      }
      case _ => {
        val ns = xmlEvent.ns
        val local = xmlEvent.local

        val arr = arrayStack.topMaybe

        var thisERD = nextElementResolver.nextElement(local, ns)

        def erdNS = thisERD.namedQName.namespace.toStringOrNullIfNoNS
        def erdLocal = thisERD.namedQName.local

        //
        // Now we're ready to handle this specific XML Event
        // once we get here, the ERD must have ns and local that match the incoming event
        //
        Assert.invariant(ns =:= erdNS)
        Assert.invariant(local =:= erdLocal)

        xmlEvent match {
          case _: EndComplex => Assert.invariantFailed("EndComplex already handled")
          case StartComplex(ns, local) => {
            nextElementResolver = thisERD.childElementResolver
            val node = new DIComplex(thisERD)
            nodeStack.top.addChild(node)
            nodeStack.push(node)
            accumulatedEvents += Start(node)
          }
          case Simple(ns, local, text) if (thisERD.isSimpleType) => {
            nextElementResolver = thisERD.nextElementResolver
            val node = new DISimple(thisERD)
            nodeStack.top.addChild(node)
            val primType = thisERD.optPrimType.get
            val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(text)
            val obj = primType.fromXMLString(remapped)
            node.setDataValue(obj)
            accumulatedEvents += Start(node)
            accumulatedEvents += End(node)
          }
          case Simple(ns, local, text) if (thisERD.isComplexType) => {
            //
            // This case comes up when a complexType has only an array child
            // and that array has zero elements. The non-schema-aware
            // XML stuff can't tell the difference between this and an 
            // element with simple content
            //
            nextElementResolver = thisERD.nextElementResolver
            val node = new DIComplex(thisERD)
            nodeStack.top.addChild(node)
            accumulatedEvents += Start(node)
            accumulatedEvents += End(node)
          }
          case NilElt(ns, local) => {
            nextElementResolver = thisERD.nextElementResolver
            val node = if (thisERD.isSimpleType) new DISimple(thisERD) else new DIComplex(thisERD)
            nodeStack.top.addChild(node)
            node.setNilled()
            accumulatedEvents += Start(node)
            accumulatedEvents += End(node)
          }
        }

        val Start(node: DIElement) = accumulatedEvents.head
        (thisERD.isArray, arr.toScalaOption) match {
          case (true, Some(diArray)) => {
            Assert.invariant(diArray.totalElementCount > 0)
            if (diArray(1).runtimeData =:= thisERD) {
              // same array, another element
              // so do nothing
            } else {
              // end one array and start another immediately (adjacent array elements)
              val newDIArray: DIArray = node.parent.get.getChildArray(thisERD).get.asInstanceOf[DIArray]
              arrayStack.popMaybe
              arrayStack.pushMaybe(One(newDIArray))
              accumulatedEvents.insert(0, End(diArray))
              accumulatedEvents.insert(1, Start(newDIArray))
            }
          }
          case (true, None) => {
            // prior was not an array (or there is no prior - first in group)
            // so start an array
            val newDIArray: DIArray = node.parent.get.getChildArray(thisERD).get.asInstanceOf[DIArray]
            arrayStack.popMaybe
            arrayStack.pushMaybe(One(newDIArray))
            accumulatedEvents.insert(0, Start(newDIArray))
          }
          case (false, None) => {
            // not an array, prior not an array. No array transitiion
            // do nothing
          }
          case (false, Some(diArray)) => {
            arrayStack.popMaybe
            arrayStack.pushMaybe(Nope)
            accumulatedEvents.insert(0, End(diArray))
          }
        }

        if (xmlEvent.isInstanceOf[StartComplex]) {
          arrayStack.pushMaybe(Nope)
        }
      }
    }
  }

  override def hasNext = {
    if (savedEvents.length > 0) true
    else xmlIterator.hasNext
  }

}
