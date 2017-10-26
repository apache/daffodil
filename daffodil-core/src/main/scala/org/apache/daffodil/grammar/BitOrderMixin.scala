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

package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.equality.TypeEqual
import edu.illinois.ncsa.daffodil.grammar.primitives.BitOrderChange
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

trait BitOrderMixin extends GrammarMixin with ByteOrderAnalysisMixin { self: Term =>

  protected final lazy val optDefaultBitOrder: Option[BitOrder] = {
    val bitOrd =
      if (tunable.requireBitOrderProperty) {
        Some(bitOrder)
      } else {
        optionBitOrder
      }
    bitOrd
  }

  final lazy val defaultBitOrder = optDefaultBitOrder.getOrElse(BitOrder.MostSignificantBitFirst)

  final protected lazy val isKnownSameBitOrder: Boolean = {
    val res =
      if (enclosingTerm.isEmpty) false // root needs bit order
      else {
        val optPrior = this.nearestPriorPhysicalTermSatisfying(_.optDefaultBitOrder.isDefined)
        optPrior match {
          case None => false // no prior that has a bit order we could be the same as
          case Some(prior) => {
            if (prior.defaultBitOrder =:= this.defaultBitOrder) true
            else false
          }
        }
      }
    res
  }

  protected lazy val hasUniformBitOrderThroughout: Boolean = {
    val res = termChildren.map { t => t.isKnownSameBitOrder && t.hasUniformBitOrderThroughout }.forall(x => x)
    res
  }

  protected final lazy val needsBitOrderChange = {
    enclosingTerm.isEmpty || (
      optionBitOrder.isDefined &&
        thereIsAByteOrderDefined && // if there is no byte order, then there's no need for bit order. The two go together. An all-textual format doesn't need either one.
        (!isKnownSameBitOrder ||
          (isArray && !hasUniformBitOrderThroughout)))
  }

  protected final lazy val bitOrderChange = prod("bitOrderChange", needsBitOrderChange) {
    BitOrderChange(this)
  }

}
