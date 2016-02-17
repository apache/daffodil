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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.exceptions.Assert

case class ConvertTextCombinatorUnparser(
  rd: RuntimeData,
  valueUnparser: Unparser,
  converterUnparser: Unparser)
  extends UnparserObject(rd) {

  override lazy val childProcessors = Seq(converterUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    converterUnparser.unparse1(state, rd)

    if (state.status ne Success) return

    valueUnparser.unparse1(state, rd)
  }
}

case class ConvertTextNumberUnparser[S](
  helper: ConvertTextNumberParserUnparserHelperBase[S],
  nff: NumberFormatFactoryBase[S],
  erd: ElementRuntimeData)
  extends TermUnparser(erd)
  with ToBriefXMLImpl {

  override def toString = "to(xs:" + helper.xsdType + ")"
  override lazy val childProcessors = Nil

  lazy val zeroRep: Maybe[String] = helper.zeroRepListRaw.headOption.map { zr =>
    EntityReplacer { _.replaceForUnparse(zr) }
  }

  def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue

    // The type of value should have the type of S, but type erasure makes this
    // difficult to assert. Could probably check this with TypeTags or Manifest
    // if we find this is not the case. Want something akin to:
    // Assert.invariant(value.isInstanceOf[S])

    val strRep =
      if (value == 0 && zeroRep.isDefined) {
        zeroRep.get
      } else {
        val df = nff.getNumFormat(state)
        df.get.format(value)
      }

    node.overwriteDataValue(strRep)
  }
}
