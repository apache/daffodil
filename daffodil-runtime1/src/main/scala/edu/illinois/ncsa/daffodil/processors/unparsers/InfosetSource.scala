package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.DIDocument
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData

object InfosetSource {

  def fromInfosetTree(infoset: InfosetDocument): InfosetSource = {
    val is = new InfosetSourceFromTree(infoset)
    is
  }

  /**
   * Incoming XML has no notion of array. So that must be inferred so that
   * Start/EndInfosetArray are generated.
   */
  def fromXMLNode(xmlDocument: scala.xml.Node, erd: ElementRuntimeData): InfosetSource = {
    val doc = Infoset.newDocument(Infoset.elem2Infoset(erd, xmlDocument))
    val src = fromInfosetTree(doc)
    src
  }

  /**
   * Without constructing the intermediate XML Tree, that is, on the fly, generates the
   * DFDL Infoset events for the InfosetSource as they are pulled.
   *
   * This is schema aware as it must infer when arrays are starting and ending
   * in order to generate those events as well as the startElement endElement events.
   */
  def fromXMLSource(xmlStreamReader: javax.xml.stream.XMLStreamReader, schemaInfo: DPathElementCompileInfo): InfosetSource = {
    ???
  }

}

/**
 * Iterates an infoset tree, handing out elements one by one in response to pull calls.
 *
 * Assumes that arrays have already been recognized and turned into DIArray nodes.
 */
class InfosetSourceFromTree(doc: InfosetDocument) extends InfosetSource {
  val diDoc = doc.asInstanceOf[DIDocument]
  var currentDINode: DINode = diDoc.root
  var currentChild = -1
  var numCurrentChildren = 0
  val diStack = new mutable.Stack[DINode]()
  val childIndexStack = new mutable.Stack[Long]
  val arrayIndexStack = new mutable.Stack[Long]

  /**
   * Easiest way to pull is to use a on-demand created stream.
   */
  final def hasNext(): Boolean = !stream.isEmpty

  def next(): InfosetEvent = {
    Assert.usage(hasNext())
    val hd = stream.head
    stream = stream.tail
    println("EVENT: " + hd)
    hd
  }

  var stream: Stream[InfosetEvent] = {
    val rootElem = diDoc.root
    f(rootElem, Stream.Empty)
  }

  private def f(diNode: DINode, more: Stream[InfosetEvent]): Stream[InfosetEvent] = {
    val rest = End(diNode) #:: more
    val childrenAndRest = diNode.children.foldRight(rest) { f }
    val first = Start(diNode)
    val result = first #:: childrenAndRest
    result
  }

}

trait InfosetSource extends Iterator[InfosetEvent]

sealed abstract class InfosetEvent(val node: DINode)
case class End(override val node: DINode) extends InfosetEvent(node)
case class Start(override val node: DINode) extends InfosetEvent(node)

