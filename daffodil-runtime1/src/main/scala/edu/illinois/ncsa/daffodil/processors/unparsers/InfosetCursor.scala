package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetDocument
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.util.Cursor
import edu.illinois.ncsa.daffodil.util.Accessor
import edu.illinois.ncsa.daffodil.util.IteratorWithPeek
import edu.illinois.ncsa.daffodil.xml._

class InfosetError(kind: String, args: String*) extends ProcessingError("Infoset Error", Nope, Nope, kind, args: _*)

object InfosetSource {

  def fromInfosetTree(infoset: InfosetDocument): InfosetSource = {
    val is = new InfosetSourceFromTree(infoset)
    is
  }

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
  def fromXMLSource(xmlEventCursor: XMLEventCursor, rootElementERD: ElementRuntimeData): InfosetSource = {
    val orig = new InfosetSourceFromXMLEventCursor(xmlEventCursor, rootElementERD)
    orig
  }

}

// TODO: Rename to InfosetCursor (and rename file too)
trait InfosetSource extends Cursor[InfosetEvent] {

  /**
   * Override these further only if you want to say, delegate from one cursor implemtation
   * to another one.
   */
  override val advanceAccessor = new InfosetEvent(null, null)
  override val inspectAccessor = new InfosetEvent(null, null)
}

sealed trait InfosetEventKind
case object StartKind extends InfosetEventKind
case object EndKind extends InfosetEventKind
// case object SimpleKind extends InfosetEventKind

/**
 * An infoset event object is an infoset event accessor.
 */
case class InfosetEvent(var kind: InfosetEventKind, var node: DINode) extends Accessor[InfosetEvent] {
  def namedQName = node.namedQName
  def erd: ElementRuntimeData = node match {
    case a: DIArray => a.parent.runtimeData
    case e: DIElement => e.runtimeData
  }

  override def toString = {
    val knd = if (kind eq null) "NullKind" else kind
    val lbl = knd.toString.dropRight(4)
    lbl + "(" + node + ")"
  }

  def isStart = kind == StartKind
  def isEnd = kind == EndKind

  override def cpy() = copy()
  override def assignFrom(other: InfosetEvent) {
    kind = other.kind
    node = other.node
  }
}

object Start {
  def apply(node: DINode) = InfosetEvent(StartKind, node)
  def unapply(ev: InfosetEvent) = if (ev.kind eq StartKind) Some(ev.node) else None
}

object End {
  def apply(node: DINode) = InfosetEvent(EndKind, node)
  def unapply(ev: InfosetEvent) = if (ev.kind eq EndKind) Some(ev.node) else None
}

//object Simple {
//  def apply(node: DINode) = InfosetEvent(SimpleKind, node)
//  def unapply(ev: InfosetEvent) = if (ev.kind eq SimpleKind) Some(ev.node) else None
//}
