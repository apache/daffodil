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

import edu.illinois.ncsa.daffodil.exceptions.Assert

import NodeInfo._

object NodeInfoUtils {

  /**
   * Generalizes the type of the two alternatives of an if-then-else
   */
  def generalizeIfThenElse(thenExpr: Expression, elseExpr: Expression): NodeInfo.Kind = {
    val a = thenExpr.inherentType
    val b = elseExpr.inherentType
    if (a == b) a
    else if (a.isSubtypeOf(b)) b
    else if (b.isSubtypeOf(a)) a
    else
      (a, b) match {
        case (s: String.Kind, _) => String
        case (_, s: String.Kind) => String
        case (Float, Double) => Double
        case (Double, Float) => Double
        case (Decimal, Double) => Decimal
        case (Double, Decimal) => Decimal
        case (Boolean, bt: Numeric.Kind) => bt
        case (bt: Numeric.Kind, Boolean) => bt
        case (it: Long.Kind, ArrayIndex) => ArrayIndex
        case _ => thenExpr.SDE("Static type error: expressions '%s' and '%s' have incompatible types %s and %s.", thenExpr.text, elseExpr.text, a, b)
      }
  }

  /**
   * For a comparison operator, compute type to which the args should be converted
   */
  def generalizeArgTypesForComparisonOp(op: String,
    inherent1: Numeric.Kind,
    inherent2: Numeric.Kind): Numeric.Kind = {

    val argType: Numeric.Kind = (inherent1, inherent2) match {
      case (x, y) if (x eq y) => x
      case (_, Decimal) => Decimal
      case (Decimal, _) => Decimal
      case (_, Double) => Double
      case (Double, _) => Double
      case (_, Float) => Double
      case (Float, _) => Double
      case (_, Integer) => Integer
      case (Integer, _) => Integer
      case (_, NonNegativeInteger) => Integer
      case (NonNegativeInteger, _) => Integer
      case (_: UnsignedLong.Kind, UnsignedLong) => UnsignedLong
      case (UnsignedLong, _: UnsignedLong.Kind) => UnsignedLong
      case (_, UnsignedLong) => Integer
      case (UnsignedLong, _) => Integer
      case (_, ArrayIndex) => ArrayIndex
      case (ArrayIndex, _) => ArrayIndex
      case (_, Long) => Long
      case (Long, _) => Long
      case (_, UnsignedInt) => Long
      case (UnsignedInt, _) => Long
      case (_, Int) => Int
      case (Int, _) => Int
      case (_, UnsignedShort) => Int
      case (UnsignedShort, _) => Int
      case (_, Short) => Short
      case (Short, _) => Short
      case (_, UnsignedByte) => Short
      case (UnsignedByte, _) => Short
      case (_, Byte) => Byte
      case (Byte, _) => Byte
      case _ => Assert.usageError(
        "Unsupported types for comparison op '%s' were %s and %s.".format(op, inherent1, inherent2))
    }
    argType
  }

  /**
   * For a numeric operation, compute types the args should be converted to, and the resulting type
   * from the operation on them.
   */
  def generalizeArgAndResultTypesForNumericOp(op: String,
    leftArgType: Numeric.Kind,
    rightArgType: Numeric.Kind): ( //
    Numeric.Kind, // first result is generalized arg type
    Numeric.Kind // second result is generalized result type
    ) = {
    /*
     * Adjust for the Decimal result type when div is used on any integer types.
     */
    def divResult(t: NodeInfo.Numeric.Kind) =
      if (op == "div") Decimal else t

    val (argType: Numeric.Kind, resultType: Numeric.Kind) = (leftArgType, rightArgType) match {
      case (_, Decimal) => (Decimal, Decimal)
      case (Decimal, _) => (Decimal, Decimal)
      case (_, Double) => (Double, Double)
      case (Double, _) => (Double, Double)
      case (_, Float) => (Double, Double)
      case (Float, _) => (Double, Double)
      case (_, Integer) => (Integer, divResult(Integer))
      case (Integer, _) => (Integer, divResult(Integer))
      case (_, NonNegativeInteger) => (NonNegativeInteger, divResult(Integer))
      case (NonNegativeInteger, _) => (NonNegativeInteger, divResult(Integer))
      case (_, UnsignedLong) => (UnsignedLong, divResult(Integer))
      case (UnsignedLong, _) => (UnsignedLong, divResult(Integer))
      case (_, ArrayIndex) => (ArrayIndex, ArrayIndex)
      case (ArrayIndex, _) => (ArrayIndex, ArrayIndex)
      case (_, Long) => (Long, divResult(Long))
      case (Long, _) => (Long, divResult(Long))
      case (_, UnsignedInt) => (UnsignedInt, divResult(Long))
      case (UnsignedInt, _) => (UnsignedInt, divResult(Long))
      case (_, Int) => (Int, divResult(Int))
      case (Int, _) => (Int, divResult(Int))
      case (_, UnsignedShort) => (UnsignedShort, divResult(Int))
      case (UnsignedShort, _) => (UnsignedShort, divResult(Int))
      case (_, Short) => (Short, divResult(Int))
      case (Short, _) => (Short, divResult(Int))
      case (_, UnsignedByte) => (UnsignedByte, divResult(Int))
      case (UnsignedByte, _) => (UnsignedByte, divResult(Int))
      case (_, Byte) => (Byte, divResult(Int))
      case (Byte, _) => (Byte, divResult(Int))
      case _ => Assert.usageError(
        "Unsupported types for numeric op '%s' were %s and %s.".format(op, leftArgType, rightArgType))
    }
    (argType, resultType)
  }
}
