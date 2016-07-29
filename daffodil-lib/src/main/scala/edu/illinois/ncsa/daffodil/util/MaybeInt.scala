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
 * Uses a Long to store an Int so that we can still pass by Value, but we
 * can reserve a value to represent Nope.
 */
final case class MaybeInt private (__v: Long) extends AnyVal {
  @inline final def get: Int = if (isDefined) __v.toInt else noneGet
  @inline final def getOrElse(alternate: Int): Int = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeInt.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeInt(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUInt.Nope
  // verbose but known-to-be-fast
}

object MaybeInt {
  private val undefValue = Long.MaxValue

  @inline final def apply(v: Int) = new MaybeInt(v)

  val Nope = new MaybeInt(undefValue)
}

final case class MaybeChar private (__v: Int) extends AnyVal {
  @inline final def get: Char = if (isDefined) __v.toChar else noneGet
  @inline final def getOrElse(alternate: Char): Char = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeChar.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeChar(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUChar.Nope
  // verbose but known-to-be-fast
}

object MaybeChar {
  private val undefValue = -1

  @inline final def apply(v: Char) = new MaybeChar(v)

  val Nope = new MaybeChar(undefValue)
}

final case class MaybeBoolean private (__v: Int) extends AnyVal {
  @inline final def get: Boolean = if (isEmpty) noneGet else __v == 1
  @inline final def getOrElse(alternate: Boolean): Boolean = if (isDefined) get else alternate
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __v != MaybeBoolean.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "MaybeBoolean(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeUBoolean.Nope
  // verbose but known-to-be-fast
}

object MaybeBoolean {
  private val undefValue = -1

  @inline final def apply(v: Boolean) = new MaybeBoolean(if (v) 1 else 0)

  val Nope = new MaybeBoolean(undefValue)
}