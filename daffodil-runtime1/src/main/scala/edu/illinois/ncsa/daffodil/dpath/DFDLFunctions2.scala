/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.processors.{ DIElement, DISimple, DIComplex }
import java.lang.{ Long => JLong }
import passera.unsigned.ULong

sealed abstract class DFDLLengthFunctionBase(kind: String, recipes: List[CompiledDPath]) extends FNTwoArgsNodeAndValue(recipes) {

  /**
   * The base behavior is content length, because we need that for contentLength (obviously), but
   * also for valueLength of complex types.
   */
  protected def getLength(elt: DIElement, units: LengthUnits, dstate: DState): ULong = {
    // TODO: retry/resume loop
    // so this will block until the contentLength is computable.
    //
    val len: ULong =
      DState.withRetryIfBlocking(dstate) {
        units match {
          case LengthUnits.Bits => elt.contentLengthInBits
          case LengthUnits.Bytes => elt.contentLengthInBytes
          case LengthUnits.Characters =>
            // TODO: assert check for scannable so length in characters makes sense.
            ??? // elt.contentLengthInCharacters
        }
      }
    len
  }

  override def computeValue(anyNode: AnyRef, str: AnyRef, dstate: DState): AnyRef = {

    val elt = anyNode match {
      case e: DIElement => e
      case _ => throw new IllegalArgumentException("dfdl:%sLength's first argument must be an Infoset Element. Argument was: %s".format(kind, anyNode))
    }

    val units = str match {
      case s: String => LengthUnits(s, elt.runtimeData)
      case _ => throw new IllegalArgumentException("dfdl:%sLength's second argument must be one of the strings 'bits', 'bytes', or 'characters', but was: %s.".format(kind, str))
    }

    val jLen: JLong = getLength(elt, units, dstate).longValue
    jLen
  }
}
case class DFDLContentLength(recipes: List[CompiledDPath])
  extends DFDLLengthFunctionBase("content", recipes)

case class DFDLValueLength(recipes: List[CompiledDPath])
  extends DFDLLengthFunctionBase("value", recipes) {

  override def getLength(eltArg: DIElement, units: LengthUnits, dstate: DState) = {
    eltArg match {
      case elt: DISimple => {
        val len = units match {
          case LengthUnits.Bits => ???
          case LengthUnits.Bytes => ???
          case LengthUnits.Characters =>
            // TODO: assert check for scannable so length in characters makes sense.
            // or do we error if charset not fixed width ??
            ???
        }
        len
      }
      case c: DIComplex => super.getLength(eltArg, units, dstate)
    }
  }
}
