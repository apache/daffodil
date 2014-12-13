package edu.illinois.ncsa.daffodil.xml

/**
 * Adapted from Scala libraries so their copyright is preserved here.
 *
 * Copyright (C) 2014, Tresys Technology. All rights reserved.
 *
 * This file exists to overcome a bug in the original scala libarary pretty printer
 * which is illustrated in unit test test_scala_xml_pretty_printer_normalizes_whitespace_inside_cdata_bug.
 */

import scala.xml._
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import scala.collection.mutable
import parsing.XhtmlEntities
import scala.language.implicitConversions

/**
 * The `Utility` object provides utility functions for processing instances
 * of bound and not bound XML classes, as well as escaping text nodes.
 *
 * @author Burak Emir
 */
object Utility extends AnyRef with parsing.TokenTests {
  final val SU = '\u001A'

  // [Martin] This looks dubious. We don't convert StringBuilders to
  // Strings anywhere else, why do it here?
  implicit def implicitSbToString(sb: StringBuilder) = sb.toString()

  // helper for the extremely oft-repeated sequence of creating a
  // StringBuilder, passing it around, and then grabbing its String.
  private[xml] def sbToString(f: (StringBuilder) => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
  private[xml] def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]

  /**
   * Trims an element - call this method, when you know that it is an
   *  element (and not a text node) so you know that it will not be trimmed
   *  away. With this assumption, the function can return a `Node`, rather
   *  than a `Seq[Node]`. If you don't know, call `trimProper` and account
   *  for the fact that you may get back an empty sequence of nodes.
   *
   *  Precondition: node is not a text node (it might be trimmed)
   */
  def trim(x: Node): Node = x match {
    case Elem(pre, lab, md, scp, child @ _*) =>
      Elem(pre, lab, md, scp, (child flatMap trimProper): _*)
  }

  /**
   * trim a child of an element. `Attribute` values and `Atom` nodes that
   *  are not `Text` nodes are unaffected.
   */
  def trimProper(x: Node): Seq[Node] = x match {
    case Elem(pre, lab, md, scp, child @ _*) =>
      Elem(pre, lab, md, scp, (child flatMap trimProper): _*)
    case Text(s) =>
      new TextBuffer().append(s).toText
    case _ =>
      x
  }

  /** returns a sorted attribute list */
  def sort(md: MetaData): MetaData = if ((md eq Null) || (md.next eq Null)) md else {
    val key = md.key
    val smaller = sort(md.filter { m => m.key < key })
    val greater = sort(md.filter { m => m.key > key })
    smaller.foldRight(md copy greater)((x, xs) => x copy xs)
  }

  /**
   * Return the node with its attribute list sorted alphabetically
   *  (prefixes are ignored)
   */
  def sort(n: Node): Node = n match {
    case Elem(pre, lab, md, scp, child @ _*) =>
      Elem(pre, lab, sort(md), scp, (child map sort): _*)
    case _ => n
  }

  /**
   * Escapes the characters &lt; &gt; &amp; and &quot; from string.
   */
  final def escape(text: String): String = sbToString(escape(text, _))

  object Escapes {
    /**
     * For reasons unclear escape and unescape are a long ways from
     * being logical inverses.
     */
    val pairs = Map(
      "lt" -> '<',
      "gt" -> '>',
      "amp" -> '&',
      "quot" -> '"' // enigmatic comment explaining why this isn't escaped --
      // is valid xhtml but not html, and IE doesn't know it, says jweb
      // "apos"  -> '\''
      )
    val escMap = pairs map { case (s, c) => c -> ("&%s;" format s) }
    val unescMap = pairs ++ Map("apos" -> '\'')
  }
  import Escapes.{ escMap, unescMap }

  /**
   * Appends escaped string to `s`.
   */
  final def escape(text: String, s: StringBuilder): StringBuilder = {
    // Implemented per XML spec:
    // http://www.w3.org/International/questions/qa-controls
    // imperative code 3x-4x faster than current implementation
    // dpp (David Pollak) 2010/02/03
    val len = text.length
    var pos = 0
    while (pos < len) {
      text.charAt(pos) match {
        case '<' => s.append("&lt;")
        case '>' => s.append("&gt;")
        case '&' => s.append("&amp;")
        case '"' => s.append("&quot;")
        case '\n' => s.append('\n')
        case '\r' => s.append('\r')
        case '\t' => s.append('\t')
        case c => if (c >= ' ') s.append(c)
      }

      pos += 1
    }
    s
  }

  /**
   * Appends unescaped string to `s`, `amp` becomes `&amp;`,
   * `lt` becomes `&lt;` etc..
   *
   * @return    `'''null'''` if `ref` was not a predefined entity.
   */
  final def unescape(ref: String, s: StringBuilder): StringBuilder =
    ((unescMap get ref) map (s append _)).orNull

  /**
   * Returns a set of all namespaces used in a sequence of nodes
   * and all their descendants, including the empty namespaces.
   */
  def collectNamespaces(nodes: Seq[Node]): mutable.Set[String] =
    nodes.foldLeft(new mutable.HashSet[String]) { (set, x) => collectNamespaces(x, set); set }

  /**
   * Adds all namespaces in node to set.
   */
  def collectNamespaces(n: Node, set: mutable.Set[String]) {
    if (n.doCollectNamespaces) {
      set += n.namespace
      for (a <- n.attributes) a match {
        case _: PrefixedAttribute =>
          set += a.getNamespace(n)
        case _ =>
      }
      for (i <- n.child)
        collectNamespaces(i, set)
    }
  }

  // def toXML(
  //   x: Node,
  //   pscope: NamespaceBinding = TopScope,
  //   sb: StringBuilder = new StringBuilder,
  //   stripComments: Boolean = false,
  //   decodeEntities: Boolean = true,
  //   preserveWhitespace: Boolean = false,
  //   minimizeTags: Boolean = false): String =
  // {
  //   toXMLsb(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
  //   sb.toString()
  // }

  /**
   * Serialize the provided Node to the provided StringBuilder.
   * <p/>
   * Note that calling this source-compatible method will result in the same old, arguably almost universally unwanted,
   * behaviour.
   */
  @deprecated("Please use `serialize` instead and specify a `minimizeTags` parameter", "2.10.0")
  def toXML(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = false): StringBuilder =
    {
      serialize(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, if (minimizeTags) MinimizeMode.Always else MinimizeMode.Never)
    }

  /**
   * Serialize an XML Node to a StringBuilder.
   *
   * This is essentially a minor rework of `toXML` that can't have the same name due to an unfortunate
   * combination of named/default arguments and overloading.
   *
   * @todo use a Writer instead
   */
  def serialize(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default): StringBuilder =
    {
      x match {
        case c: Comment if !stripComments => c buildString sb
        // 
        // If it's plain Text node, then unless preserve whitespace we have to 
        // normalize whitespace.
        case s: Text =>
          if (preserveWhitespace) s buildString sb
          else sb.append(TextBuffer.fromString(s.toString).sb)
        //
        // other textual nodes (PCData, PI, etc.) we always preserve whitespace.
        //
        case s: SpecialNode => s buildString sb
        case g: Group =>
          for (c <- g.nodes) serialize(c, g.scope, sb, minimizeTags = minimizeTags); sb
        case el: Elem =>
          // print tag with namespace declarations
          sb.append('<')
          el.nameToString(sb)
          if (el.attributes ne null) el.attributes.buildString(sb)
          el.scope.buildString(sb, pscope)
          if (el.child.isEmpty &&
            (minimizeTags == MinimizeMode.Always ||
              (minimizeTags == MinimizeMode.Default && el.minimizeEmpty))) {
            // no children, so use short form: <xyz .../>
            sb.append("/>")
          } else {
            // children, so use long form: <xyz ...>...</xyz>
            sb.append('>')
            //
            // changed here to pass the additional flags so this knows 
            // what to do recursively with whitespace and entities.
            //
            sequenceToXML(el.child, el.scope, sb, stripComments,
              decodeEntities, preserveWhitespace, minimizeTags)
            sb.append("</")
            el.nameToString(sb)
            sb.append('>')
          }
        case _ => throw new IllegalArgumentException("Don't know how to serialize a " + x.getClass.getName)
      }
    }

  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default): Unit =
    {
      if (children.isEmpty) return
      else if (children forall isAtomAndNotText) { // add space
        val it = children.iterator
        val f = it.next
        serialize(f, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
        while (it.hasNext) {
          val x = it.next
          if (!preserveWhitespace) sb.append(' ') // only if we're not preserving whitespace.
          serialize(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
        }
      } else children foreach { serialize(_, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }
    }

  /**
   * Returns prefix of qualified name if any.
   */
  final def prefix(name: String): Option[String] = (name indexOf ':') match {
    case -1 => None
    case i => Some(name.substring(0, i))
  }

  /**
   * Returns a hashcode for the given constituents of a node
   */
  def hashCode(pre: String, label: String, attribHashCode: Int, scpeHash: Int, children: Seq[Node]) =
    scala.util.hashing.MurmurHash3.orderedHash(label +: attribHashCode +: scpeHash +: children, pre.##)

  def appendQuoted(s: String): String = sbToString(appendQuoted(s, _))

  /**
   * Appends &quot;s&quot; if string `s` does not contain &quot;,
   * &apos;s&apos; otherwise.
   */
  def appendQuoted(s: String, sb: StringBuilder) = {
    val ch = if (s contains '"') '\'' else '"'
    sb.append(ch).append(s).append(ch)
  }

  /**
   * Appends &quot;s&quot; and escapes and &quot; i s with \&quot;
   */
  def appendEscapedQuoted(s: String, sb: StringBuilder): StringBuilder = {
    sb.append('"')
    for (c <- s) c match {
      case '"' =>
        sb.append('\\'); sb.append('"')
      case _ => sb.append(c)
    }
    sb.append('"')
  }

  def getName(s: String, index: Int): String = {
    if (index >= s.length) null
    else {
      val xs = s drop index
      if (xs.nonEmpty && isNameStart(xs.head)) xs takeWhile isNameChar
      else ""
    }
  }

  /**
   * Returns `'''null'''` if the value is a correct attribute value,
   * error message if it isn't.
   */
  def checkAttributeValue(value: String): String = {
    var i = 0
    while (i < value.length) {
      value.charAt(i) match {
        case '<' =>
          return "< not allowed in attribute value";
        case '&' =>
          val n = getName(value, i + 1)
          if (n eq null)
            return "malformed entity reference in attribute value [" + value + "]";
          i = i + n.length + 1
          if (i >= value.length || value.charAt(i) != ';')
            return "malformed entity reference in attribute value [" + value + "]";
        case _ =>
      }
      i = i + 1
    }
    null
  }

  def parseAttributeValue(value: String): Seq[Node] = {
    val sb = new StringBuilder
    var rfb: StringBuilder = null
    val nb = new NodeBuffer()

    val it = value.iterator
    while (it.hasNext) {
      var c = it.next
      // entity! flush buffer into text node
      if (c == '&') {
        c = it.next
        if (c == '#') {
          c = it.next
          val theChar = parseCharRef({ () => c }, { () => c = it.next }, { s => throw new RuntimeException(s) }, { s => throw new RuntimeException(s) })
          sb.append(theChar)
        } else {
          if (rfb eq null) rfb = new StringBuilder()
          rfb append c
          c = it.next
          while (c != ';') {
            rfb.append(c)
            c = it.next
          }
          val ref = rfb.toString()
          rfb.clear()
          unescape(ref, sb) match {
            case null =>
              if (sb.length > 0) { // flush buffer
                nb += Text(sb.toString())
                sb.clear()
              }
              nb += EntityRef(ref) // add entityref
            case _ =>
          }
        }
      } else sb append c
    }
    if (sb.length > 0) { // flush buffer
      val x = Text(sb.toString())
      if (nb.length == 0)
        return x
      else
        nb += x
    }
    nb
  }

  /**
   * {{{
   *   CharRef ::= "&amp;#" '0'..'9' {'0'..'9'} ";"
   *             | "&amp;#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   * }}}
   * See [66]
   */
  def parseCharRef(ch: () => Char, nextch: () => Unit, reportSyntaxError: String => Unit, reportTruncatedError: String => Unit): String = {
    val hex = (ch() == 'x') && { nextch(); true }
    val base = if (hex) 16 else 10
    var i = 0
    while (ch() != ';') {
      ch() match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + ch().asDigit
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
          | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if (!hex)
            reportSyntaxError("hex char not allowed in decimal char ref\n" +
              "Did you mean to write &#x ?")
          else
            i = i * base + ch().asDigit
        case SU =>
          reportTruncatedError("")
        case _ =>
          reportSyntaxError("character '" + ch() + "' not allowed in char ref\n")
      }
      nextch()
    }
    new String(Array(i), 0, 1)
  }
}

import Utility._
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
    val sb = new StringBuilder()
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
    node.attribute(XML.namespace, XML.space).map(_.toString == XML.preserve) getOrElse false

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
    format(n, null, sb)
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
  def format(n: Node, pscope: NamespaceBinding = null): String =
    sbToString(format(n, pscope, _))

  /**
   * Returns a formatted string containing well-formed XML.
   *
   *  @param nodes  the sequence of nodes to be serialized
   *  @param pscope the namespace to prefix mapping
   */
  def formatNodes(nodes: Seq[Node], pscope: NamespaceBinding = null): String =
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
