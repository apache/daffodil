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

object NodeInfoUtils {
  /**
   * When operating on two operands, this computes the type to which
   * they are mutually converted before the operation. Such as if you
   * add an Int and a Double, the Int is converted to Double before adding, and
   * the result type is Double.
   */
  def generalize(aExpr: Expression, bExpr: Expression): NodeInfo.Kind = {
    val a = aExpr.inherentType
    val b = bExpr.inherentType
    if (a == b) a
    else if (a.isSubtypeOf(b)) b
    else if (b.isSubtypeOf(a)) a
    else
      (a, b) match {
        case (s: NodeInfo.String.Kind, _) => NodeInfo.String
        case (_, s: NodeInfo.String.Kind) => NodeInfo.String
        case (NodeInfo.Float, NodeInfo.Double) => NodeInfo.Double
        case (NodeInfo.Double, NodeInfo.Float) => NodeInfo.Double
        case (NodeInfo.Decimal, NodeInfo.Double) => NodeInfo.Decimal
        case (NodeInfo.Double, NodeInfo.Decimal) => NodeInfo.Decimal
        case (NodeInfo.Boolean, bt: NodeInfo.Numeric.Kind) => bt
        case (bt: NodeInfo.Numeric.Kind, NodeInfo.Boolean) => bt
        case (it: NodeInfo.Long.Kind, NodeInfo.ArrayIndex) => NodeInfo.ArrayIndex
        case _ => aExpr.SDE("Static type error: expressions '%s' and '%s' have incompatible types %s and %s.", aExpr.text, bExpr.text, a, b)
      }
  }
}