/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.OutputNewLineEv
import org.apache.daffodil.processors.InfosetCachedEvaluatable
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.cookers.EntityReplacer

class NilStringLiteralForUnparserEv(trd: TermRuntimeData,
  maybeOutputNewLineEv: Maybe[OutputNewLineEv],
  stringLiteralRaw: String)
  extends Evaluatable[String](trd)
  with InfosetCachedEvaluatable[String] {

  override lazy val runtimeDependencies = maybeOutputNewLineEv.toList

  override protected def compute(state: ParseOrUnparseState): String = {
    val endMarker = "__daffodil_stringLiteralForUnparser_endMarker__"
    Assert.invariant(!stringLiteralRaw.endsWith(endMarker))
    val rawWithEndMark = stringLiteralRaw + endMarker
    EntityReplacer { er =>
      val cookedWithEndMark = er.replaceForUnparse(rawWithEndMark)
      val chunksSeparatedByNL = cookedWithEndMark.split(er.markerForNL).toSeq
      val last = chunksSeparatedByNL.last
      val butLast = chunksSeparatedByNL.take(chunksSeparatedByNL.length - 1)
      val chunks =
        if (last == endMarker) {
          // this means the original string ended with a %NL;
          // because the endMarker is all by itself.
          butLast :+ "" // replace it by an empty string
        } else {
          // the original string did not end with a %NL;
          // so that means whatever the last token is, it has our endMarker
          // glued onto it, and we must remove that.
          Assert.invariant(last.endsWith(endMarker))
          butLast :+ last.replace(endMarker, "")
        }

      if (chunks.length == 1) {
        // there are no NL entities. There is only a single chunk.
        chunks.head
      } else {
        trd.schemaDefinitionUnless(maybeOutputNewLineEv.isDefined,
          "Property dfdl:outputNewLine is required, but it is not defined.")
        val nl = maybeOutputNewLineEv.get.evaluate(state)
        val sl = chunks.mkString(nl)
        sl
      }
    }
  }
}
