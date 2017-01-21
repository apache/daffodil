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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors.parsers.ParseError
import edu.illinois.ncsa.daffodil.util.Maybe
import Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError

case class DAFTrace(recipe: CompiledDPath, msg: String)
    extends FNOneArg(recipe, NodeInfo.AnyType) {

  override def computeValue(str: AnyRef, dstate: DState) = {
    System.err.println("trace " + msg + ":" + str.toString)
    str
  }
}

case object DAFError extends RecipeOp {

  override def run(dstate: DState) {
    val maybeSFL =
      if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
      else Nope
    dstate.mode match {
      case UnparserNonBlocking | UnparserBlocking =>
        UnparseError(maybeSFL, dstate.contextLocation, "The error function was called.")
      case _ : ParserMode => {
        val pe = new ParseError(maybeSFL, dstate.contextLocation, "The error function was called.")
        throw pe
      }
    }
  }
}
