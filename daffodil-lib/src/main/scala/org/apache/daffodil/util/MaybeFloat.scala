/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.util

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
  @inline final def get: Double = if (isDefined) java.lang.Double.longBitsToDouble(__rep) else noneGet
  //@inline final def getOrElse(alternate: Double): Double = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeDouble.undefValue
  @inline final def isEmpty = !isDefined
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
  @inline final def get: Float = if (isDefined) java.lang.Float.intBitsToFloat(__rep.toInt) else noneGet
  //@inline final def getOrElse(alternate: Float): Float = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeFloat.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeFloat(" + get + ")"
}

object MaybeFloat {

  private val undefValue = -1L

  @inline final def apply(v: Float) = new MaybeFloat(java.lang.Float.floatToRawIntBits(v).toLong)

  val Nope = new MaybeFloat(undefValue)
}
