/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dpath

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dpath.NodeInfo

import NodeInfo.*

object NodeInfoUtils {

  /**
   * For a comparison operator, compute type to which the args should be converted
   */
  def generalizeArgTypesForComparisonOp(
    op: String,
    inherent1: Numeric.Kind,
    inherent2: Numeric.Kind
  ): Numeric.Kind = {

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
      case _ =>
        Assert.usageError(
          "Unsupported types for comparison op '%s' were %s and %s.".format(
            op,
            inherent1,
            inherent2
          )
        )
    }
    argType
  }

  /**
   * For a numeric operation, compute types the args should be converted to, and the resulting type
   * from the operation on them.
   */
  def generalizeArgAndResultTypesForNumericOp(
    op: String,
    leftArgType: Numeric.Kind,
    rightArgType: Numeric.Kind
  ): ( //
    Numeric.Kind, // first result is generalized arg type
    Numeric.Kind // second result is generalized result type
  ) = {

    // When doing arithmetic, we first find the least upper bound of the left and right arg
    // types. If the least upper bound can fit into a long, we promote both args to a long,
    // which reduces the likelihood of accidental overflow/underflow during intermediate
    // arithmetic operations, especially with smaller types like xs:byte. The long result will
    // likely need to be downcast to the final result type at the end of the expression, at
    // which point we check for overflow/underflow errors.
    val lub = NodeInfoUtils.typeLeastUpperBound(leftArgType, rightArgType)
    val argType = lub match {
      case ArrayIndex => Long
      case SignedNumeric => {
        // SignedNumeric is the least upper bound of float, double, and decimal/subtypes. One of
        // the args must be a float or double, so we must promote to either Double or Decimal.
        // If one of the args is arbitrary width/precision (i.e. non-negative integer or a
        // parent type) then we promote the args to Decimal to maintain that level of precision.
        // Otherwise, it means both args are finite width/precision and we promote both args to
        // Double. This provides efficient math and maintains limited precision operations while
        // still having reasonably good precision. This also maintains compatibility with older
        // version of Daffodil that did most math using Doubles
        if (
          NonNegativeInteger.isSubtypeOf(leftArgType) ||
          NonNegativeInteger.isSubtypeOf(rightArgType)
        ) {
          Decimal
        } else {
          Double
        }
      }
      case Decimal => Decimal
      case Double => Double
      case Float => Float
      case _: Long.Kind => Long
      case _: UnsignedInt.Kind => Long
      case _: Numeric.Kind => Integer
      // $COVERAGE-OFF$
      case _ =>
        Assert.invariantFailed(
          s"least upper bound of $leftArgType and $rightArgType was not a numeric: $lub"
        )
      // $COVERAGE-ON$
    }

    // Determine the return type of the numeric operation.
    //
    // Division returns either Decimal, Double, or Float. Note that we return Double when the
    // converged arg type is Long to ensure reasonable precision and performance. We avoid using
    // Decimal since that has additional overhead that we try to avoid unless a schema
    // explicitly uses larger Decimal/Integer/etc types.
    //
    // Integer division returns Integer or Long, again preferring Long for performance reasons
    // unless a schema explicitly uses larger types.
    //
    // All other numeric operations return the same type as the converged args
    val resultType = op match {
      case "div" =>
        argType match {
          case Decimal => Decimal
          case Integer => Decimal
          case Double => Double
          case Float => Float
          case Long => Double
          // $COVERAGE-OFF$
          case _ => Assert.invariantFailed(s"unexpected arg type: $argType")
          // $COVERAGE-ON$
        }
      case "idiv" =>
        argType match {
          case Decimal => Integer
          case Integer => Integer
          case Double => Long
          case Float => Long
          case Long => Long
          // $COVERAGE-OFF$
          case _ => Assert.invariantFailed(s"unexpected arg type: $argType")
          // $COVERAGE-ON$
        }
      case _ => argType
    }

    (argType, resultType)
  }

  def typeLeastUpperBound(left: NodeInfo.Kind, right: NodeInfo.Kind): NodeInfo.Kind = {
    if (left.isSubtypeOf(right)) right
    else if (right.isSubtypeOf(left)) left
    else {
      val leftParents = left.parents
      val leftLubs = leftParents.map { typeLeastUpperBound(right, _) }
      Assert.invariant(leftLubs.length == 1) // our type lattice has this property.
      val res = leftLubs.head
      res
    }
  }
}
