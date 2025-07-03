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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable

object Range {
  type Range = (RangeBound, RangeBound)
  def inclusive(lower: DataValuePrimitiveNullable, upper: DataValuePrimitiveNullable): Range = {
    val lower_ = new RangeBound(lower, true)
    val upper_ = new RangeBound(upper, true)
    (lower_, upper_)
  }
}

/*
 * This would be a good example of why we might want DataValue to have a more rich
 * hierarchy. In theory, RangeBound could be defined in term of a DataValueNumeric type,
 * from which all of the numeric types extend. In practice, getting complicated DataValue hierarchies to work
 * turns out to be annoyingly finicky, and so may not be worth the benifit.
 *
 * Additionally, in the current implementation, all instance of RangeBound actually use JBigInt.
 * This is because most of the current type calculator implementation is based on casting all numeric types
 * to BigInt, instead of trying to keep track of what types we would be expecting at runtime.
 */
class RangeBound(val maybeBound: DataValuePrimitiveNullable, val isInclusive: Boolean)
  extends Serializable {

  lazy val isEmpty = maybeBound.isEmpty
  lazy val isDefined = maybeBound.isDefined

  override def toString(): String = {
    if (maybeBound.isDefined) {
      maybeBound.getAnyRef.toString + "_" + (if (isInclusive) "inclusive" else "exclusive")
    } else {
      "Nope"
    }
  }

  def intersectsWithOtherBounds(lower: RangeBound, upper: RangeBound): Boolean = {
    if (maybeBound.isEmpty) {
      false
    } else {
      val bound = maybeBound.getNonNullable
      val inBounds = lower.testAsLower(bound) && upper.testAsUpper(bound)
      val atBoundery = maybeBound == lower.maybeBound || maybeBound == upper.maybeBound
      inBounds && (isInclusive || !atBoundery)
    }
  }

  /*
   * It should be the case that x is either a JJBigInt, or a Long
   */

  def testAsLower(x: DataValuePrimitive): Boolean = {
    if (maybeBound.isEmpty) {
      true
    } else {
      val bound = maybeBound.getNonNullable
      if (isInclusive) {
        le(bound, x)
      } else {
        lt(bound, x)
      }
    }
  }
  def testAsUpper(x: DataValuePrimitive): Boolean = {
    if (maybeBound.isEmpty) {
      true
    } else {
      val bound = maybeBound.getNonNullable
      if (isInclusive) {
        le(x, bound)
      } else {
        lt(x, bound)
      }
    }
  }

  private def le(x: DataValuePrimitive, y: DataValuePrimitive): Boolean = {
    val x2 = Numbers.asBigInt(x.getAnyRef)
    val y2 = Numbers.asBigInt(y.getAnyRef)

    x2.compareTo(y2) <= 0
  }

  private def lt(x: DataValuePrimitive, y: DataValuePrimitive): Boolean = {
    val x2 = Numbers.asBigInt(x.getAnyRef)
    val y2 = Numbers.asBigInt(y.getAnyRef)

    x2.compareTo(y2) < 0
  }

}
