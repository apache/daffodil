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

import edu.illinois.ncsa.daffodil.util.Indentable
import edu.illinois.ncsa.daffodil.xml.XMLUtils

/**
 * Writes the infoset to a java.io.Writer as XML text.
 *
 * @param writer The writer to write the XML text to
 * @param pretty Whether or to enable pretty printing. Set to true, XML
 *               elements are indented and newlines are inserted.
 */
class XMLTextInfosetOutputter(writer: java.io.Writer, pretty: Boolean = true)
  extends InfosetOutputter with Indentable with XMLInfosetOutputter {

  override def reset(): Unit = {
    resetIndentation()
  }

  private def getTagName(elem: DIElement): String = {
    val prefix = elem.erd.thisElementsNamespacePrefix
    if (prefix == null || prefix == "") elem.erd.name else prefix + ":" + elem.erd.name
  }

  private def outputStartTag(elem: DIElement, name: String): Unit = {
    writer.write("<")

    writer.write(name)

    val nsbStart = elem.erd.minimizedScope
    val nsbEnd = if (elem.isRoot) scala.xml.TopScope else elem.diParent.erd.minimizedScope
    if (nsbStart != nsbEnd) {
      val str = XMLUtils.uniqueNamespaceBindingsToString(nsbStart, nsbEnd)
      writer.write(" ")
      writer.write(str)
    }

    if (elem.isNilled) {
      writer.write(" xsi:nil\"=true\"")
    }

    writer.write(">")
  }

  private def outputEndTag(elem: DIElement, name: String): Unit = {
    writer.write("</")
    writer.write(name)
    writer.write(">")
  }

  override def startSimple(simple: DISimple): Boolean = {
    if (pretty) outputIndentation(writer)
    val name = getTagName(simple)
    outputStartTag(simple, name)

    if (!simple.isNilled && simple.hasValue) {
      // TODO: some quick and dirty performance testing shows that this remap
      // and escape is somewhat expensive. This needs to be confirmed, but we
      // should perhaps only do them if we first detect characters that either
      // need to be remapped or escaped. This should make the common case of no
      // weird characters faster. Or perhaps just need to rewrite them to be
      // faster and combine the two functions. Need to confirm and profile
      val s = remapped(simple.dataValueAsString)
      val escaped = scala.xml.Utility.escape(s)
      writer.write(escaped)
    }

    outputEndTag(simple, name)
    if (pretty) writer.write(System.lineSeparator())
    true
  }
  
  override def endSimple(simple: DISimple): Boolean = {
    // do nothing, everything is done in startSimple
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    val name = getTagName(complex)
    if (pretty) outputIndentation(writer)
    outputStartTag(complex, name)
    incrementIndentation()
    if (pretty) writer.write(System.lineSeparator())
    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    val name = getTagName(complex)
    decrementIndentation()
    if (pretty) outputIndentation(writer)
    outputEndTag(complex, name)
    if (pretty) writer.write(System.lineSeparator())
    true
  }

  override def startArray(array: DIArray): Boolean = {
    // do nothing
    true
  }

  override def endArray(array: DIArray): Boolean = {
    // do nothing
    true
  }

  override def startDocument(): Boolean = {
    // do nothing
    true
  }

  override def endDocument(): Boolean = {
    // do nothing
    true
  }
}
