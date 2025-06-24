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

package org.apache.daffodil.lib.xml

import scala.xml._

import org.apache.daffodil.lib.exceptions.Assert

/**
 * Trivial pretty printer.
 *
 * Indents for start of elements, never does anything with content of simple elements, so those always print on one line
 * with start and end tags on the same line.
 */
class PrettyPrinter(step: Int) {

  private def isLeaf(l: Node) = l match {
    case _: Atom[_] | _: Comment | _: EntityRef | _: ProcInstr => true
    case _ => false
  }

  private def allAreLeaves(ns: Seq[Node]): Boolean = {
    ns.forall(isLeaf)
  }

  /**
   * Appends a formatted string containing well-formed XML with
   *  given namespace to prefix mapping to the given string buffer.
   *
   * @param n    the node to be serialized
   * @param sb   the stringbuffer to append to
   */
  def format(n: Node, sb: StringBuilder): Unit = { // entry point
    val c = minimizeScopes(n)
    format(c, sb, 0)
  }

  /**
   * Very specific to this pretty print algorithm.
   *
   * This traverses the node, and for any element, removes any xmlns bindings
   * that are not needed when this is displayed.
   *
   * XML that has been processed through this method can then be printed
   * element by element. I.e., one can grab a child element and toString it,
   * and it will print only the xmlns bindings that would have been printed
   * had one printed that child in the context of printing its enclosing parent
   * node.
   *
   * This is incorrect from the standpoint of XML printing strictly speaking.
   * That is, suppose
   * {{{
   * val xml =
   * <x:foo xmlns:x="xNamespace">
   *    <x:bar>5</x:bar>
   * </x:foo>
   * }}}
   * If you do {{{
   * xml.child.toString
   * }}} you will get {{{
   * "<x:bar xmlns:x='xNamespace'>5</x:bar>"
   * }}} however, if you minimize scopes {{{
   * val xml = minimizeScopes(
   * <x:foo xmlns:x="xNamespace">
   *    <x:bar>5</x:bar>
   * </x:foo>
   * )
   * xml.child.toString
   * }}}
   * You will get {{{
   * "<x:bar>5</x:bar>
   * }}}
   * because the namespace binding for xmlns:x on x:bar isn't needed if the whole xml
   * is printed.
   */
  private def minimizeScopes(n: Node): Node = {
    minimizeScopes1(n, Seq(TopScope))
  }

  private def scopeToList(nb: NamespaceBinding): Seq[NamespaceBinding] = {
    if (nb eq TopScope) Seq(TopScope)
    else nb +: scopeToList(nb.parent)
  }

  private def listToScope(nbs: Seq[NamespaceBinding]): NamespaceBinding = {
    if (nbs.isEmpty) TopScope
    else {
      val nb = nbs.head
      val rest = nbs.tail
      new NamespaceBinding(nb.prefix, nb.uri, listToScope(rest))
    }
  }

  private def minimizeScopes1(n: Node, nbs: Seq[NamespaceBinding]): Node = {
    n match {
      case e: Elem => {
        val nScopeList = scopeToList(e.scope)
        val newNScopeList = nScopeList.filter { x =>
          !nbs.exists(nb => nb.prefix == x.prefix && nb.uri == x.uri)
        } // scope without anything defined in nbs
        val newNScope = listToScope(newNScopeList)
        val combinedNScopeList = newNScopeList ++ nbs
        val minimizedChildren = e.child.map { minimizeScopes1(_, combinedNScopeList) }
        val newNode = e.copy(scope = newNScope, child = minimizedChildren)
        newNode
      }
      case x => x
    }
  }

  private val mtChild = <foo></foo>.child

  private def format(n: Node, sb: StringBuilder, cur: Int): Unit = { // entry point
    n match {
      case e: Elem => {
        sb.append(" " * cur)
        val child = e.child
        if (child.isEmpty || allAreLeaves(child)) {
          // simple and empty element case
          sb.append(e.toString)
        } else {
          // element-only elements (could contain processing instructions or other elements, or comments in theory)
          val emptyElem = e.asInstanceOf[Elem].copy(child = mtChild, minimizeEmpty = false)
          val tagsString: String = emptyElem.toString
          val Array(partialStartTag, partialEndTag) = tagsString.split("><")
          val startTag = partialStartTag + ">"
          val endTag = "<" + partialEndTag
          sb.append(startTag)
          child.filterNot { _.isInstanceOf[Atom[_]] }.foreach { c =>
            {
              sb.append("\n")
              format(c, sb, cur + step)
            }
          }
          sb.append("\n")
          sb.append(" " * cur)
          sb.append(endTag)
        }
      }
      case pi: ProcInstr => sb.append(n.toString)
      case co: Comment => sb.append(n.toString)
      case _ =>
        Assert.invariantFailed(
          "XML infoset node did not contain only other elements, PI or comments: " + n
        )
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
  def format(n: Node): String = {
    val c = minimizeScopes(n)
    val sb = new StringBuilder
    format(c, sb, 0)
    sb.toString
  }

  /**
   * Returns a formatted string containing well-formed XML.
   *
   *  @param nodes  the sequence of nodes to be serialized
   *  @param pscope the namespace to prefix mapping
   */
  def formatNodes(nodes: Seq[Node]): String = {
    val sb = new StringBuilder
    formatNodes(nodes, sb)
    sb.toString
  }

  /**
   * Appends a formatted string containing well-formed XML with
   *  the given namespace to prefix mapping to the given stringbuffer.
   *
   *  @param nodes  the nodes to be serialized
   *  @param pscope the namespace to prefix mapping
   *  @param sb     the string buffer to which to append to
   */
  def formatNodes(nodes: Seq[Node], sb: StringBuilder): Unit =
    nodes.foreach(n => sb.append(format(n)))
}
