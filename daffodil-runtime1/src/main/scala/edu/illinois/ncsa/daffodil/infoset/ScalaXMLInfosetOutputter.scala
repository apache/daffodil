/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.util.Maybe
import scala.xml.Null
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.MStackOf

class ScalaXMLInfosetOutputter(showFormatInfo: Boolean = false) extends InfosetOutputter
    with XMLInfosetOutputter {

  val stack = new MStackOf[ListBuffer[scala.xml.Node]]
  var nodeSeq: Maybe[scala.xml.NodeSeq] = Maybe.Nope

  def reset(): Unit = {// call to reuse these. When first constructed no reset call is necessary.
    nodeSeq = Maybe.Nope
    stack.clear
  }

  def startDocument(): Boolean = {
    stack.push(new ListBuffer())
    true
  }

  def endDocument(): Boolean = {
    val root = stack.pop
    assert(root.length == 1)
    nodeSeq = Maybe.One(root(0))
    true
  }

  def startSimple(diSimple: DISimple): Boolean = {

    val e =
      if (diSimple.isNilled && diSimple.erd.isNillable) {
        scala.xml.Elem(diSimple.erd.thisElementsNamespacePrefix, diSimple.erd.name,
            XMLUtils.xmlNilAttribute, diSimple.erd.minimizedScope, true)
      } else if (diSimple.hasValue) {
        val s = remapped(diSimple.dataValueAsString)
        // At this point s contains only legal XML characters.
        // However, since we're going to create actual XML documents here,
        // we have to do escaping. There are two ways to do escaping.
        // One is to convert &, <, >, and " to &amp; &lt; &gt; &quot;.
        // The other is to wrap the contents in <![CDATA[ ...]]> brackets.
        // For strings longer than a certain size, or with a large number of
        // the characters requiring escaping,... CDATA is preferred.
        //
        // TODO: add some tunable option to control (a) PUA mapping or not
        // (b) CDATA or escapify, or CDATA for things of some size, or we
        // can put Daffodil specific annotations on the ERD e.g., daf:xmlEscapePolicy
        // with options for single chars, CDATA, or and generate always or
        // generate when needed. etc.
        //
        // Anyway... Constructing a Text node seems to automatically escapeify
        // the supplied content.
        val textNode = new scala.xml.Text(s)
        scala.xml.Elem(diSimple.erd.thisElementsNamespacePrefix, diSimple.erd.name, Null,
            diSimple.erd.minimizedScope, true, textNode)
      } else {
        // element has been created but has no value yet, display an empty element tag
        scala.xml.Elem(diSimple.erd.thisElementsNamespacePrefix, diSimple.erd.name, Null,
            diSimple.erd.minimizedScope, true)
      }

    val elem = addFmtInfo(diSimple, e, showFormatInfo)

    nodeSeq = Maybe.One(elem)
    stack.top.append(elem)
    //returning true/false will be used when recursion is removed
    true
  }

  def endSimple(diSimple: DISimple): Boolean = {
    true
  }

  def startComplex(diComplex: DIComplex): Boolean = {
    stack.push(new ListBuffer())
    true
  }

  def endComplex(diComplex: DIComplex): Boolean = {

    val children = stack.pop

    val e =
      if (diComplex.isNilled && diComplex.erd.isNillable) {
        scala.xml.Elem(diComplex.erd.thisElementsNamespacePrefix, diComplex.erd.name,
          XMLUtils.xmlNilAttribute, diComplex.erd.minimizedScope, true)
      } else {
        scala.xml.Elem(diComplex.erd.thisElementsNamespacePrefix, diComplex.erd.name,
            scala.xml.Null, diComplex.erd.minimizedScope, true, children: _*)
      }

    val elem = addFmtInfo(diComplex, e, showFormatInfo)

    nodeSeq = Maybe.One(elem)
    stack.top.append(elem)
    //returning true/false will be used when recursion is removed
    true
  }

  def startArray(diArray: DIArray): Boolean = {
    // Array elements are started individually
    true
  }
  def endArray(diArray: DIArray): Boolean = {
    true
  }

  def getResult(): Maybe[scala.xml.NodeSeq] = {
    nodeSeq
  }

  
}

