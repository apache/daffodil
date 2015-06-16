/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

/**
 * A PrimParser is a parser that contains no child parsers.
 * Combinators are NOT PrimParser
 */
abstract class PrimParser(contextArg: RuntimeData)
  extends DaffodilParser(contextArg)
  with WithParseErrorThrowing {

  override protected def childProcessors = Nil

}

/**
 * A PrimUnparser is an unparser that contains no child unparsers.
 * Combinators are not PrimUnparsers
 */
abstract class PrimUnparser(contextArg: RuntimeData)
  extends DaffodilUnparser(contextArg) {

  override lazy val childProcessors = Nil

}

class NadaParser(context: RuntimeData) extends PrimParser(context) {
  override def toString = "Nada"

  def parse(start: PState): Unit = {
    // do nothing
  }
}

class NadaUnparser(context: RuntimeData) extends PrimUnparser(context) {
  override def toString = "Nada"

  def unparse(start: UState) = {
    // do nothing
  }
}