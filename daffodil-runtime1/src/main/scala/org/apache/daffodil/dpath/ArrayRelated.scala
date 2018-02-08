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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.infoset.InfosetNoInfosetException
import edu.illinois.ncsa.daffodil.util.Maybe.Nope

case class FNCount(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe)
  with ExistsKind {

  override def run(dstate: DState) {
    val res = exists(recipe, dstate)
    if (res) {
      dstate.mode match {
        case UnparserBlocking | UnparserNonBlocking => {
          // we are unparsing, so asking for an array count is asking for the
          // final count, not however many are in there at this point
          // so we have to know if the array is closed or not.
          dstate.setCurrentValue(dstate.finalArrayLength)
        }
        case _: ParserMode => {
          dstate.setCurrentValue(dstate.arrayLength)
        }
      }
    } else {
      dstate.setCurrentValue(0)
    }
  }
}

/**
 * Returns \$arg if it contains exactly one item. Otherwise, raises an error
 */
case object FNExactlyOne extends RecipeOp {
  override def run(dstate: DState) {

    //      // Original implementation with boolean return-type
    ////    val bool = dstate.arrayLength == 1
    ////    dstate.setCurrentValue(bool)
    //
    //    // New implementation as stated in tickets DFDL-1085/1087
    //    val hasExactlyOne = dstate.arrayLength == 1
    //    if (hasExactlyOne) {
    //      val array = dstate.currentArray
    //      val item = array.getOccurrence(1).asInstanceOf[DINode]
    //
    //      dstate.setCurrentNode(item)
    //    }
    //    else { throw new Exception("fn:exactly-one called with a sequence containing zero or more than one item.") }

    //
    //This was commented out in 6c9612e9f711beb1d8e4239ef9a473eb9c64a32f, which as the commit message:
    //
    //    Implements fn:exactly-one function, but comments out the functionality as the function
    //    is useless until query-style path expressions are allowed/implemented. Yields a subsetError
    //    until such time.

    // The reason queries are needed is that without them, well, the answer to this has to be true, as
    // there are no functions which return zero nor more than one node.
    //
    dstate.SDE("Subset - fn:exactly-one is not implemented by Daffodil.")
  }
}

case object DFDLOccursIndex extends RecipeOp {
  override def run(dstate: DState) {
    Assert.invariant(dstate.arrayPos >= 1)
    if (dstate.isCompile)
      throw new InfosetNoInfosetException(Nope)
    dstate.setCurrentValue(dstate.arrayPos)
  }
}
