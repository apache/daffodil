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

package org.apache.daffodil.infoset

import org.apache.daffodil.util.Maybe
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.equality._


trait XMLInfosetOutputter {

  def remapped(dataValueAsString: String) = XMLUtils.remapXMLIllegalCharactersToPUA(dataValueAsString)

  /**
   * String suitable for use in the text of a Processing Instruction.
   *
   * The text is a pseudo-XML string.
   */
  protected final def fmtInfo(diTerm: DITerm): Maybe[String] = {
    // val mgXML = modelGroupsPseudoXML
    val pecXML = diTerm.parserEvalCache.toPseudoXML()
    val uecXML = diTerm.unparserEvalCache.toPseudoXML()
    val puxml = {
      // mgXML +
      (if (pecXML =:= "") "" else "\n" + pecXML) +
        (if (uecXML =:= "") "" else "\n" + uecXML)
    }
    Maybe(if (puxml =:= "") null else puxml)
  }

  final def addFmtInfo(diTerm: DITerm, elem: scala.xml.Elem, showFormatInfo: Boolean): scala.xml.Elem = {
    if (!showFormatInfo) return elem
    val maybeFI = fmtInfo(diTerm)
    val res =
      if (maybeFI.isEmpty) elem
      else {
        val fi = maybeFI.value
        val pi = new scala.xml.ProcInstr("formatInfo", fi)
        val res = elem.copy(child = elem.child :+ pi)
        res
      }
    res
  }

}
