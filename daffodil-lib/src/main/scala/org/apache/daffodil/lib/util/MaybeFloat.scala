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

package org.apache.daffodil.lib.util

/**
 * Uses a Long to store the bits of a Double so that we can compare
 * to a single specific bit-pattern NaN as the Nope indicator.
 *
 * Note that this does not preserve every bit. There is a single distinguished NaN
 * value which cannot be preserved bit for bit, as it is the one used to represent Nope.
 *
 * So this class cannot be used to represent a Double that is extracted from the bits
 * of a data stream, for example, because it is possible (though unlikely) that these
 * bits are those of this distinguished NaN value.
 */
final case class MaybeDouble private (__rep: Long) extends AnyVal {
  @inline final def value = get
  @inline final def get: Double =
    if (isDefined) java.lang.Double.longBitsToDouble(__rep) else noneGet
  // @inline final def getOrElse(alternate: Double): Double = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeDouble.undefValue
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Double] = if (isDefined) Some(get) else None
  override def toString = if (isEmpty) "Nope" else "MaybeDouble(" + get + ")"
  // @inline final def map(f: Double => Double): MaybeDouble = if (isEmpty) MaybeDouble.Nope else MaybeDouble(f(get))
}

object MaybeDouble {

  private val undefValue = {
    val aRandomDouble = 0.6026145343615157 // math.random
    val minNaNAsLong = 0xff800001L
    val aRandomNaNAsLong = java.lang.Double.doubleToRawLongBits(aRandomDouble) | minNaNAsLong
    val signallingNaNWithAllOnesPayload = java.lang.Double.longBitsToDouble(aRandomNaNAsLong)
    val result = java.lang.Double.doubleToRawLongBits(signallingNaNWithAllOnesPayload)
    result
  }

  @inline final def apply(v: Double) = new MaybeDouble(java.lang.Double.doubleToRawLongBits(v))

  val Nope = new MaybeDouble(undefValue)
}

/**
 * Uses a Long to store an Float so that we can still pass by Value, but we
 * can reserve a value to represent Nope.
 */
final case class MaybeFloat private (__rep: Long) extends AnyVal {
  @inline final def get: Float =
    if (isDefined) java.lang.Float.intBitsToFloat(__rep.toInt) else noneGet
  // @inline final def getOrElse(alternate: Float): Float = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeFloat.undefValue
  @inline final def isEmpty = !isDefined
  @inline final def toOption: Option[Float] = if (isDefined) Some(get) else None
  override def toString = if (isEmpty) "Nope" else "MaybeFloat(" + get + ")"
}

object MaybeFloat {

  private val undefValue = -1L

  @inline final def apply(v: Float) = new MaybeFloat(
    java.lang.Float.floatToRawIntBits(v).toLong
  )

  val Nope = new MaybeFloat(undefValue)
}
