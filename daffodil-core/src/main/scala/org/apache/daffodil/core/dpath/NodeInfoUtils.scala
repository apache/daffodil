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

import NodeInfo._

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

    /*
     * Adjust for the Decimal result type when div/idiv is used
     */
    def divResult(resultType: NodeInfo.Numeric.Kind) = resultType match {
      case ArrayIndex => ArrayIndex
      case _: Decimal.Kind => Decimal
      case Double => Double
      case Float => Float
      case _ => Assert.usageError("Unsupported return type: %s".format(resultType))
    }

    def idivResult(resultType: NodeInfo.Numeric.Kind) = resultType match {
      case Decimal => Integer
      case Integer => Integer
      case Double => Long
      case Long => Long
      case Float => Int
      case Int => Int
      case Short => Short
      case ArrayIndex => ArrayIndex
      case _ => Assert.usageError("Unsupported return type: %s".format(resultType))
    }

    val (argType: Numeric.Kind, resultType: Numeric.Kind) = {
      val lub = NodeInfoUtils.typeLeastUpperBound(leftArgType, rightArgType)
      //
      // For each abstract type that could be the least upper bound of the two
      // arg types, we must pick a concrete type to convert everything into.
      val lubImplementationType = lub match {
        case SignedNumeric => NodeInfo.Double
        case _ => lub
      }
      (lubImplementationType, lubImplementationType)
    }
    val res = op match {
      case "div" => (argType, divResult(resultType))
      case "idiv" => (argType, idivResult(resultType))
      case _ => (argType, resultType)
    }
    res
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
