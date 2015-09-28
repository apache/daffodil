package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.equality._
import scala.xml.pull.XMLEvent
import scala.xml.pull._
import scala.collection.mutable

sealed abstract class XMLInfosetEvent(val ns: String, val local: String)
case class Simple(override val ns: String, override val local: String, text: String) extends XMLInfosetEvent(ns, local)
case class StartComplex(override val ns: String, override val local: String) extends XMLInfosetEvent(ns, local)
case class EndComplex(override val ns: String, override val local: String) extends XMLInfosetEvent(ns, local)
case class NilElt(override val ns: String, override val local: String) extends XMLInfosetEvent(ns, local)

private[unparsers] object XMLEventIterator {
  sealed trait Mode
  case object ElementOnly extends Mode
  case object SimpleContent extends Mode
  case object NilContent extends Mode
}

/**
 * Simplifies the stream of XMLEvent to a more-relevant stream of XMLInfosetEvent
 * which are intended to be useful for constructing the stream of actual InfosetEvent.
 *
 * This is still schema-unaware. So it can't tell whether an empty element is
 * of simple or complex type. It just knows if it appears to be simple in the XML.
 * It also doesn't try to infer array boundaries.
 */
class XMLEventIterator(xsr: Iterator[scala.xml.pull.XMLEvent]) extends Iterator[XMLInfosetEvent] {

  import XMLEventIterator._

  private lazy val iterator = trans(preprocess(xsr.toStream), new mutable.ArrayStack[scala.xml.NamespaceBinding]()).toIterator
  override def hasNext = iterator.hasNext
  override def next = iterator.next

  private[unparsers] def preprocess(xes: Stream[XMLEvent]): Stream[XMLEvent] = {
    if (xes.isEmpty) return Stream.Empty
    var str = xes
    var stop = false
    var ev: XMLEvent = null
    while (!str.isEmpty && !stop) {
      ev = str.head
      str = str.tail
      ev match {
        case _: EvComment => // skip
        case _: EvProcInstr => // skip
        case _ => stop = true
      }
    }
    ev #:: preprocess(str)
  }

  /**
   * Peeks ahead a bit to see if we're looking at a simple, element-only, or nil
   * event stream
   */
  private[unparsers] def findMode(xes: Stream[XMLEvent]): Mode = {
    xes match {
      case EvElemStart(_, _, attrs, scope) #:: EvElemStart(_, _, _, _) #:: rest => ElementOnly
      case EvElemStart(_, _, attrs, scope) #:: EvText(s) #:: EvElemStart(_, _, _, _) #:: rest => {
        if (!s.matches("""\s*""")) Assert.invariantFailed("couldn't determine Mode for " + xes)
        ElementOnly
      }
      case EvElemStart(_, _, attrs, scope) #:: EvElemEnd(_, _) #:: rest if isNil(attrs, scope) => NilContent
      case EvElemStart(_, _, attrs, scope) #:: EvElemEnd(_, _) #:: rest => SimpleContent
      case EvElemStart(_, _, _, _) #:: EvText(s) #:: EvElemEnd(_, _) #:: rest => SimpleContent
      case EvElemStart(_, _, _, _) #:: EvText(s) #:: EvElemStart(_, _, _, _) #:: rest if s.matches("""\s*""") => ElementOnly
      case EvElemStart(_, _, _, _) #:: EvText(s) #:: rest if !s.matches("""\s*""") => SimpleContent
      case EvElemStart(_, _, _, _) #:: EvEntityRef(s) #:: rest => SimpleContent
      case _ => Assert.invariantFailed("couldn't determine Mode for " + xes)
    }
  }

  private def scopeWithXSI = <foo xmlns:xsi={ XMLUtils.XSI_NAMESPACE.toString }/>.scope

  private[unparsers] def isNil(attrs: scala.xml.MetaData, scope: scala.xml.NamespaceBinding): Boolean = {
    val optNilValue = attrs.get(XMLUtils.XSI_NAMESPACE.toString, scope, "nil")
    optNilValue.map { s => s.text =:= "true" }.getOrElse {
      val optNilValue = attrs.get(XMLUtils.XSI_NAMESPACE.toString, scopeWithXSI, "nil")
      optNilValue.map { s => s.text =:= "true" }.getOrElse(false)
    }
  }

  private[unparsers] def trans(xes: Stream[XMLEvent], parentScope: mutable.ArrayStack[scala.xml.NamespaceBinding]): Stream[XMLInfosetEvent] = {
    if (xes.isEmpty) return Stream.Empty
    val hd #:: tl = xes
    hd match {
      case start @ EvElemStart(pre, label, attrs, scope) => {
        val mode = findMode(xes)
        val ns = scope.getURI(pre)
        parentScope.push(scope)
        mode match {
          case SimpleContent => accumulateSimpleContent(ns, label, tl, parentScope)
          case ElementOnly => startComplexContent(ns, label, tl, parentScope)
          case NilContent => nilContent(ns, label, tl, parentScope)
        }
      }
      case end @ EvElemEnd(pre, label) => {
        val ns = parentScope.top.getURI(pre)
        endComplexContent(ns, label, tl, parentScope)
      }
      case EvText(s) => {
        Assert.invariant(s.matches("""\s+"""))
        trans(tl, parentScope)
      }
      case x => Assert.invariantFailed("can only be start or end or all-whitespace text: " + x)
    }
  }

  private[unparsers] def nilContent(ns: String, label: String, xes: Stream[XMLEvent],
    scopeStack: mutable.ArrayStack[scala.xml.NamespaceBinding]): Stream[XMLInfosetEvent] = {
    val EvElemEnd(pre, `label`) #:: after = xes
    Assert.invariant(ns =:= scopeStack.top.getURI(pre))
    scopeStack.pop
    NilElt(ns, label) #:: trans(after, scopeStack)
  }

  private[unparsers] def accumulateSimpleContent(ns: String,
    label: String,
    tl: Stream[XMLEvent],
    scopeStack: mutable.ArrayStack[scala.xml.NamespaceBinding]): Stream[XMLInfosetEvent] = {
    val simpleParts = tl.takeWhile { ev => !ev.isInstanceOf[EvElemEnd] }.toList
    val EvElemEnd(pre, `label`) #:: after = tl.drop(simpleParts.length)
    Assert.invariant(ns =:= scopeStack.top.getURI(pre))
    val sb = new StringBuilder
    simpleParts.foreach {
      _ match {
        case EvText(s) => sb.append(s)
        case EvEntityRef(er) => sb.append(decodeEntityRef(er))
        case _ => Assert.invariantFailed("Can only be text or entity ref")
      }
    }
    scopeStack.pop
    Simple(ns, label, sb.toString) #:: trans(after, scopeStack)
  }

  private def startComplexContent(ns: String, label: String, tl: Stream[XMLEvent],
    scopeStack: mutable.ArrayStack[scala.xml.NamespaceBinding]): Stream[XMLInfosetEvent] = {
    StartComplex(ns, label) #:: trans(tl, scopeStack)
  }

  private def endComplexContent(ns: String, label: String, tl: Stream[XMLEvent],
    scopeStack: mutable.ArrayStack[scala.xml.NamespaceBinding]): Stream[XMLInfosetEvent] = {
    scopeStack.pop
    EndComplex(ns, label) #:: trans(tl, scopeStack)
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
}
