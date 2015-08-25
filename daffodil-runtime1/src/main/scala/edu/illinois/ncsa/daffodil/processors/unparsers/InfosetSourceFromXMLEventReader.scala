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

/**
 * This converter uses Scala Streams extensively, and in principle it could be
 * written to work without them by maintaining a stack, and a bunch of
 * state variables.
 *
 * This is a lot easier to get correct.
 *
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
 */
class InfosetSourceFromXMLEventReader(
  xer: Iterator[scala.xml.pull.XMLEvent],
  rootElementInfo: ElementRuntimeData) extends InfosetSource {

  private lazy val xmlIterator = new XMLEventIterator(xer)

  private val initialNextElementResolver =
    new OnlyOnePossibilityForNextElement(rootElementInfo.schemaFileLocation, rootElementInfo, RootResolver) // bootstrap

  private lazy val diDoc = new DIDocument(rootElementInfo)

  private val nodeStack = mutable.ArrayStack[DIComplex](diDoc)
  private val arrayStack = mutable.ArrayStack[Maybe[DIArray]](Nope)

  private var nextElementResolver: NextElementResolver = initialNextElementResolver

  private var savedEvents: Seq[InfosetEvent] = Seq()

  private val mtSeq = Seq()

  override def peek = {
    if (savedEvents =:= mtSeq) {
      savedEvents = nextEvents
    }
    savedEvents.head
  }

  override def next = {
    if (savedEvents =:= mtSeq) {
      savedEvents = nextEvents
    }
    val thisEvent = savedEvents.head
    savedEvents = savedEvents.tail
    thisEvent
  }

  private def nextEvents: Seq[InfosetEvent] = {
    val xmlEvent: XMLInfosetEvent = xmlIterator.next()
    xmlEvent match {
      case EndComplex(ns, local) => {
        val node = nodeStack.pop
        val arr = arrayStack.pop
        val arrayTransition =
          if (arr.isDefined)
            Seq(End(arr.get))
          else
            Seq()
        nextElementResolver = node.runtimeData.nextElementResolver // Not strict enough. This will accept children after the element.....
        arrayTransition :+ End(node)
      }
      case _ => {
        val ns = xmlEvent.ns
        val local = xmlEvent.local
        val arr = arrayStack.top
        val erd = nextElementResolver.nextElement(local, ns)
        val nodeEvents: Seq[InfosetEvent] = xmlEvent match {
          case _: EndComplex => Assert.invariantFailed("EndComplex already handled")
          case StartComplex(ns, local) => {
            nextElementResolver = erd.childElementResolver
            val node = new DIComplex(erd)
            nodeStack.top.addChild(node)
            nodeStack.push(node)
            Seq(Start(node))
          }
          case Simple(ns, local, text) if (erd.isSimpleType) => {
            nextElementResolver = erd.nextElementResolver
            val node = new DISimple(erd)
            nodeStack.top.addChild(node)
            val primType = erd.optPrimType.get
            val remapped = XMLUtils.remapPUAToXMLIllegalCharacters(text)
            val obj = primType.fromXMLString(remapped)
            node.setDataValue(obj)
            Seq(Start(node), End(node))
          }
          case Simple(ns, local, text) if (erd.isComplexType) => {
            //
            // This case comes up when a complexType has only an array child
            // and that array has zero elements. The non-schema-aware
            // XML stuff can't tell the difference between this and an 
            // element with simple content
            //
            nextElementResolver = erd.nextElementResolver
            val node = new DIComplex(erd)
            nodeStack.top.addChild(node)
            Seq(Start(node), End(node))
          }
          case NilElt(ns, local) => {
            nextElementResolver = erd.nextElementResolver
            val node = if (erd.isSimpleType) new DISimple(erd) else new DIComplex(erd)
            nodeStack.top.addChild(node)
            node.setNilled()
            Seq(Start(node), End(node))
          }
        }
        val Start(node: DIElement) :: _ = nodeEvents
        val arrayTransition =
          (erd.isArray, arr.toScalaOption) match {
            case (true, Some(diArray)) => {
              Assert.invariant(diArray.totalElementCount > 0)
              if (diArray(1).runtimeData =:= erd) {
                // same array, another element
                Nil
              } else {
                // end one array and start another immediately (adjacent array elements)
                val newDIArray: DIArray = node.parent.get.getChildArray(erd).get.asInstanceOf[DIArray]
                arrayStack.pop
                arrayStack.push(One(newDIArray))
                Seq(End(diArray), Start(newDIArray))
              }
            }
            case (true, None) => {
              // prior was not an array (or there is no prior - first in group)
              // so start an array
              val newDIArray: DIArray = node.parent.get.getChildArray(erd).get.asInstanceOf[DIArray]
              arrayStack.pop
              arrayStack.push(One(newDIArray))
              Seq(Start(newDIArray))
            }
            case (false, None) => {
              // not an array, prior not an array. No array transitiion
              Nil
            }
            case (false, Some(diArray)) => {
              arrayStack.pop
              arrayStack.push(Nope)
              Seq(End(diArray))
            }
          }

        if (xmlEvent.isInstanceOf[StartComplex]) {
          arrayStack.push(Nope)
        }

        val result = arrayTransition ++ nodeEvents
        result
      }
    }
  }

  override def hasNext = {
    if (savedEvents.length > 0) true
    else xmlIterator.hasNext
  }

}
