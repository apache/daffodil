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

package org.apache.daffodil.util

import java.lang.{ Long => JLong }
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.cookers.Converter
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.util.RangeBound.Range

object RangeBound {
  type Range[A <: AnyRef] = (RangeBound[A], RangeBound[A])
}

class RangeBound[A <: AnyRef](val maybeBound: Maybe[A], val isInclusive: Boolean)
  extends Serializable {

  lazy val isEmpty = maybeBound.isEmpty
  lazy val isDefined = maybeBound.isDefined

  override def toString(): String = {
    if (maybeBound.isDefined) {
      maybeBound.get.toString + "_" + (if (isInclusive) "inclusive" else "exclusive")
    } else {
      "Nope"
    }
  }

  def intersectsWithOtherBounds(lower: RangeBound[A], upper: RangeBound[A]): Boolean = {
    if (maybeBound.isEmpty) {
      false
    } else {
      val bound = maybeBound.get
      val inBounds = lower.testAsLower(bound) && upper.testAsUpper(bound)
      val atBoundery = maybeBound == lower.maybeBound || maybeBound == upper.maybeBound
      inBounds && (isInclusive || !atBoundery)
    }
  }

  /*
   * It should be the case that x is either a BigInt, or a Long
   */

  def testAsLower(x: A): Boolean = {
    if (maybeBound.isEmpty) {
      true
    } else {
      if (isInclusive) {
        le(maybeBound.get, x)
      } else {
        lt(maybeBound.get, x)
      }
    }
  }
  def testAsUpper(x: A): Boolean = {
    if (maybeBound.isEmpty) {
      true
    } else {
      val bound = maybeBound.get
      if (isInclusive) {
        le(x, maybeBound.get)
      } else {
        lt(x, maybeBound.get)
      }
    }
  }

  private def le(x: A, y: A): Boolean = {
    if (x.isInstanceOf[BigInt] && y.isInstanceOf[BigInt]) {
      x.asInstanceOf[BigInt] <= y.asInstanceOf[BigInt]
    } else if (x.isInstanceOf[JLong] && y.isInstanceOf[JLong]) {
      x.asInstanceOf[JLong] <= y.asInstanceOf[JLong]
    } else if (x.isInstanceOf[BigInt] && y.isInstanceOf[JLong]) {
      x.asInstanceOf[BigInt] <= BigInt(y.asInstanceOf[JLong])
    } else if (x.isInstanceOf[JLong] && y.isInstanceOf[BigInt]) {
      BigInt(x.asInstanceOf[JLong]) <= y.asInstanceOf[BigInt]
    } else {
      Assert.invariantFailed("Illegal type passed to RangeBound.le")
    }
  }

  private def lt(x: A, y: A): Boolean = {
    if (x.isInstanceOf[BigInt] && y.isInstanceOf[BigInt]) {
      x.asInstanceOf[BigInt] < y.asInstanceOf[BigInt]
    } else if (x.isInstanceOf[JLong] && y.isInstanceOf[JLong]) {
      x.asInstanceOf[JLong] < y.asInstanceOf[JLong]
    } else if (x.isInstanceOf[BigInt] && y.isInstanceOf[JLong]) {
      x.asInstanceOf[BigInt] < BigInt(y.asInstanceOf[JLong])
    } else if (x.isInstanceOf[JLong] && y.isInstanceOf[BigInt]) {
      BigInt(x.asInstanceOf[JLong]) < y.asInstanceOf[BigInt]
    } else {
      Assert.invariantFailed("Illegal type passed to RangeBound.lt")
    }
  }

}