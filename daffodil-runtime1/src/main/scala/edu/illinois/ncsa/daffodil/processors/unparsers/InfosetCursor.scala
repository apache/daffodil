package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetItem
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.util.Cursor
import edu.illinois.ncsa.daffodil.util.Accessor
import edu.illinois.ncsa.daffodil.util.IteratorWithPeek
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert

class InfosetError(kind: String, args: String*) extends ProcessingError("Infoset Error", Nope, Nope, kind, args: _*)

object InfosetCursor {

  def fromInfosetTree(infoset: InfosetItem): InfosetCursor = {
    val is = new InfosetCursorFromTree(infoset)
    is
  }

  def fromXMLNode(xmlDocument: scala.xml.Node, erd: ElementRuntimeData): InfosetCursor = {
    val doc = Infoset.newDocument(Infoset.elem2Infoset(erd, xmlDocument))
    val src = fromInfosetTree(doc)
    src
  }

  /**
   * Without constructing the intermediate XML Tree, that is, on the fly, generates the
   * DFDL Infoset events for the InfosetCursor as they are pulled.
   *
   * This is schema aware as it must infer when arrays are starting and ending
   * in order to generate those events as well as the startElement endElement events.
   */
  def fromXMLEventCursor(xmlEventCursor: XMLEventCursor, rootElementERD: ElementRuntimeData): InfosetCursor = {
    val orig = new InfosetCursorFromXMLEventCursor(xmlEventCursor, rootElementERD)
    orig
  }

}

// TODO: Rename to InfosetCursor (and rename file too)
trait InfosetCursor extends Cursor[InfosetAccessor] {

  /**
   * Override these further only if you want to say, delegate from one cursor implemtation
   * to another one.
   */
  override lazy val advanceAccessor = InfosetAccessor(null, null)
  override lazy val inspectAccessor = InfosetAccessor(null, null)
}

object NonUsableInfosetCursor extends InfosetCursor {
  private def doNotUse = Assert.usageError("Not to be called on " + Misc.getNameFromClass(this))
  override lazy val advanceAccessor = doNotUse
  override lazy val inspectAccessor = doNotUse
  override def advance = doNotUse
  override def inspect = doNotUse
}

// TODO - Performance - It's silly to use both a StartKind and EndKind accessor when
// delivering a simple type node. Separate start/end for XML makes sense because there
// are separate events for the contents between those tags, which can be widely separated.
// In Daffodil, the two events will always be back-to-back.
//
// Consider adding a SimpleKind used for simple type elements.
//
sealed trait InfosetEventKind
case object StartKind extends InfosetEventKind
case object EndKind extends InfosetEventKind
// case object SimpleKind extends InfosetEventKind

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
    val knd = if (kind eq null) "NullKind" else kind
    val lbl = knd.toString.dropRight(4)
    lbl + "(" + node + ")"
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
