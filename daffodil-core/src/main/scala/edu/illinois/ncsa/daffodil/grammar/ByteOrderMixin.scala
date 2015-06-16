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

import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.dsom.NotFound
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.processors.ByteOrderChange
import edu.illinois.ncsa.daffodil.exceptions.Assert

trait ByteOrderMixin extends GrammarMixin { self: Term =>

  final protected lazy val thereIsAByteOrderDefined: Boolean = {
    val byteOrdLookup = this.findPropertyOption("byteOrder")
    byteOrdLookup match {
      case n: NotFound => false
      case f: Found => true
    }
  }

  private lazy val isKnownSameByteOrder: Boolean = {
    val optPrior = nearestPhysicalTermSatifying(_.thereIsAByteOrderDefined)
    optPrior match {
      case None => false
      case Some(prior) => {
        val priorByteOrder = prior.byteOrder
        val thisByteOrder = byteOrder
        if (thisByteOrder.isConstant && priorByteOrder.isConstant &&
          thisByteOrder.constantAsString =:= priorByteOrder.constantAsString) true
        else false
      }
    }
  }

  private lazy val hasUniformByteOrderThroughout: Boolean = termChildren.map { t =>
    t.thereIsAByteOrderDefined && t.isKnownSameByteOrder && t.hasUniformByteOrderThroughout
  }.forall(x => x)

  protected final lazy val byteOrderChange =
    prod("byteOrderChange",
      (thereIsAByteOrderDefined &&
        !isKnownSameByteOrder) || // need to change on the way in
        (isArray && !hasUniformByteOrderThroughout)) { // need to change because of repetition
        // (when we start next iteration, it's not the same as when we started first iteration)
        // THis will SDE if there is no byte order defined for the array element (might only be byteOrder on things within the array)
        // So we're artificially requiring byte order on all arrays that do not have a uniform byte order. 
        ByteOrderChange(this)
      }
}

