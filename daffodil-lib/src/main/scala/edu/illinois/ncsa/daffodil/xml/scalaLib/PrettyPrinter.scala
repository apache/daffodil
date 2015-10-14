/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.xml.scalaLib
/* __ *\
** ________ ___ / / ___ Scala API **
** / __/ __// _ | / / / _ | (c) 2003-2013, LAMP/EPFL **
** __\ \/ /__/ __ |/ /__/ __ | http://scala-lang.org/ **
** /____/\___/_/ |_/____/_/ | | **
** |/ **
\* */

import Utility.sbToString
import scala.xml._

/**
 * Class for pretty printing. After instantiating, you can use the
 *  format() and formatNode() methods to convert XML to a formatted
 *  string. The class can be reused to pretty print any number of
 *  XML nodes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 *
 *  @param width the width to fit the output into
 *  @param step  indentation
 */
class PrettyPrinter(width: Int, step: Int) {

  class BrokenException() extends java.lang.Exception

  class Item
  case object Break extends Item {
    override def toString() = "\\"
  }
  case class Box(col: Int, s: String) extends Item
  case class Para(s: String) extends Item

  protected var items: List[Item] = Nil

  protected var cur = 0

  protected def reset() = {
    cur = 0
    items = Nil
  }

  /**
   * Try to cut at whitespace.
   */
  protected def cut(s: String, ind: Int): List[Item] = {
    val tmp = width - cur
    if (s.length <= tmp)
      return List(Box(ind, s))
    // val sb = new StringBuilder()
    var i = s indexOf ' '
    if (i > tmp || i == -1) throw new BrokenException() // cannot break

    var last: List[Int] = Nil
    while (i != -1 && i < tmp) {
      last = i :: last
      i = s.indexOf(' ', i + 1)
    }
    var res: List[Item] = Nil
    while (Nil != last) try {
      val b = Box(ind, s.substring(0, last.head))
      cur = ind
      res = b :: Break :: cut(s.substring(last.head, s.length), ind)
      // backtrack
      last = last.tail
    } catch {
      case _: BrokenException => last = last.tail
    }
    throw new BrokenException()
  }

  /**
   * Try to make indented box, if possible, else para.
   */
  protected def makeBox(ind: Int, s: String) =
    if (cur + s.length > width) { // fits in this line
      items ::= Box(ind, s)
      cur += s.length
    } else try cut(s, ind) foreach (items ::= _) // break it up
    catch { case _: BrokenException => makePara(ind, s) } // give up, para

  // dont respect indent in para, but afterwards
  protected def makePara(ind: Int, s: String) = {
    items = Break :: Para(s) :: Break :: items
    cur = ind
  }

  protected def makeCDATA(ind: Int, s: String) = {
    items = Para(s) :: items
    val cdataLineLengths = s.split("\\n").toList.map { _.length }
    val firstLineAdjustedLength = cdataLineLengths.head + ind
    val adjustedLineLengths = firstLineAdjustedLength :: cdataLineLengths.tail
    cur = math.max(adjustedLineLengths.last, ind)
  }

  // respect indent
  protected def makeBreak() = { // using wrapping here...
    items = Break :: items
    cur = 0
  }

  protected def leafTag(n: Node) = {
    def mkLeaf(sb: StringBuilder) {
      sb append '<'
      n nameToString sb
      n.attributes buildString sb
      sb append "/>"
    }
    sbToString(mkLeaf)
  }

  protected def startTag(n: Node, pscope: NamespaceBinding): (String, Int) = {
    var i = 0
    def mkStart(sb: StringBuilder) {
      sb append '<'
      n nameToString sb
      i = sb.length + 1
      n.attributes buildString sb
      n.scope.buildString(sb, pscope)
      sb append '>'
    }
    (sbToString(mkStart), i)
  }

  protected def endTag(n: Node) = {
    def mkEnd(sb: StringBuilder) {
      sb append "</"
      n nameToString sb
      sb append '>'
    }
    sbToString(mkEnd)
  }

  protected def childrenAreLeaves(n: Node): Boolean = {
    def isLeaf(l: Node) = l match {
      case _: Atom[_] | _: Comment | _: EntityRef | _: ProcInstr => true
      case _ => false
    }
    n.child forall isLeaf
  }

  protected def fits(test: String) =
    test.length < width - cur

  private def doPreserve(node: Node) =
    node.isInstanceOf[PCData] ||
      (node.attribute(XML.namespace, XML.space).map(_.toString == XML.preserve) getOrElse false)

  protected def traverse(node: Node, pscope: NamespaceBinding, ind: Int): Unit = node match {

    case PCData(s) => makeCDATA(ind, node.toString)
    case Text(s) if s.trim() == "" =>
      ;
    case Text(s) =>
      // Text nodes must be considered whitespace normalized.
      // (individually)
      makeBox(ind, TextBuffer.fromString(node.toString.trim).sb.toString)
    case _: Atom[_] | _: Comment | _: EntityRef | _: ProcInstr =>
      makeBox(ind, node.toString.trim())
    case g @ Group(xs) =>
      traverse(xs.iterator, pscope, ind)
    case _ =>
      val test = {
        val sb = new StringBuilder()
        //
        // Changed here so that the serialize routine has the responsibility
        // to normalize whitespace (or not) depending on what node type the
        // content is (recursively), and whether the xml:space='preserve' 
        // attribute is present
        // 
        // This trick, of trying to use the Utility.serialize form of the 
        // data as the ruler to see if it fits appropriately within the indentation
        // scheme.... can't work given that some content is multi-line (CDATA) and
        // line breaks cannot be avoided in those cases. 
        Utility.serialize(node, pscope, sb,
          stripComments = false,
          decodeEntities = true,
          preserveWhitespace = doPreserve(node))
        sb.toString
      }
      if (node.child.length > 0 &&
        node.child.forall { _.isInstanceOf[PCData] }) {
        // all children are CDATA sections
        makeBox(ind, test)
      } else if (node.child.length > 0 &&
        node.child.exists { _.isInstanceOf[PCData] }) {
        //
        // Some children are CDATA sections, but not all.
        //
        // In theory we can put line-breaks anywhere that
        // there is existing whitespace outside of these CData sections.
        //
        // But not between any tag or non-whitespace token and an adjacent CDATA start or end.
        // CDATA sections must stick to whatever non-whitespace things they
        // are adjacent to. 
        //
        // Unfortunately, this breaks the whole induction pattern that this traverser uses.
        // because it is assuming context-freedom which simply isn't there for CDATA. Really we 
        // need to walk the children keeping track of whether we have to stick to the 
        // preceding thing (not insert whitespace) or not.
        // 
        // Because this is getting too complex and really means refactoring the 
        // algorithm, we punt here and just output
        // the whole thing, not as pretty as it could be, but correct in that it
        // doesn't break anything about CDATA content.
        // 
        makeBox(ind, test)
      } else if (childrenAreLeaves(node) && fits(test)) {
        makeBox(ind, test)
      } else {
        val (stg, len2) = startTag(node, pscope)
        val etg = endTag(node)
        if (stg.length < width - cur) { // start tag fits
          makeBox(ind, stg)
          makeBreak()
          traverse(node.child.iterator, node.scope, ind + step)
          makeBox(ind, etg)
        } else if (len2 < width - cur) {
          // <start label + attrs + tag + content + end tag
          makeBox(ind, stg.substring(0, len2))
          makeBreak() // todo: break the rest in pieces
          /*{ //@todo
             val sq:Seq[String] = stg.split(" ");
             val it = sq.iterator;
             it.next;
             for (c <- it) {
               makeBox(ind+len2-2, c)
               makeBreak()
             }
             }*/
          makeBox(ind, stg.substring(len2, stg.length))
          makeBreak()
          traverse(node.child.iterator, node.scope, ind + step)
          makeBox(cur, etg)
          makeBreak()
        } else { // give up
          makeBox(ind, test)
          makeBreak()
        }
      }
  }

  protected def traverse(it: Iterator[Node], scope: NamespaceBinding, ind: Int): Unit =
    for (c <- it) {
      traverse(c, scope, ind)
      makeBreak()
    }

  /**
   * Appends a formatted string containing well-formed XML with
   *  given namespace to prefix mapping to the given string buffer.
   *
   * @param n    the node to be serialized
   * @param sb   the stringbuffer to append to
   */
  def format(n: Node, sb: StringBuilder) { // entry point
    format(n, TopScope, sb)
  }

  def format(n: Node, pscope: NamespaceBinding, sb: StringBuilder) { // entry point
    var lastwasbreak = false
    reset()
    traverse(n, pscope, 0)
    var cur = 0
    for (b <- items.reverse) b match {
      case Break =>
        if (!lastwasbreak) sb.append('\n') // on windows: \r\n ?
        lastwasbreak = true
        cur = 0
      //        while (cur < last) {
      //          sb append ' '
      //          cur += 1
      //        }

      case Box(i, s) =>
        lastwasbreak = false
        while (cur < i) {
          sb append ' '
          cur += 1
        }
        sb.append(s)
      case Para(s) =>
        lastwasbreak = false
        sb append s
    }
  }

  // public convenience methods

  /**
   * Returns a formatted string containing well-formed XML with
   *  given namespace to prefix mapping.
   *
   *  @param n      the node to be serialized
   *  @param pscope the namespace to prefix mapping
   *  @return      the formatted string
   */
  def format(n: Node, pscope: NamespaceBinding = TopScope): String =
    sbToString(format(n, pscope, _))

  /**
   * Returns a formatted string containing well-formed XML.
   *
   *  @param nodes  the sequence of nodes to be serialized
   *  @param pscope the namespace to prefix mapping
   */
  def formatNodes(nodes: Seq[Node], pscope: NamespaceBinding = TopScope): String =
    sbToString(formatNodes(nodes, pscope, _))

  /**
   * Appends a formatted string containing well-formed XML with
   *  the given namespace to prefix mapping to the given stringbuffer.
   *
   *  @param nodes  the nodes to be serialized
   *  @param pscope the namespace to prefix mapping
   *  @param sb     the string buffer to which to append to
   */
  def formatNodes(nodes: Seq[Node], pscope: NamespaceBinding, sb: StringBuilder): Unit =
    nodes foreach (n => sb append format(n, pscope))
}
