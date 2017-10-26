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

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.Assert
import passera.unsigned.ULong

/**
 * if isDefined then an unsigned long
 *
 * Use to replace untyped use of Long where -1 means undefined.
 * So forgetting to test for the -1 case is not possible. You must
 * call the get method to get the unvarnished ULong value, and then ask for the
 * toLong of that if you want a plain Long.
 */
final class MaybeULong private (val __rep: Long) extends AnyVal {
  @inline final def get: Long = if (isDefined) __rep else noneGet
  @inline final def getULong: ULong = ULong(__rep)
  //@inline final def getOrElse(alternate: Long): Long = if (isDefined) get else alternate
  //@inline final def getULongOrElse(alternate: ULong): ULong = ULong(getOrElse(alternate.toLong))
  private def noneGet = throw new NoSuchElementException("Nope.get")
  @inline final def isDefined = __rep != MaybeULong.undefValue
  @inline final def isEmpty = !isDefined
  override def toString = if (isEmpty) "Nope" else "One(" + get + ")"

  // No map function or other monad features because we don't want usage
  // to EVER involve allocating closures/anonymous functions.
  //
  // The work-around: write an if-then-else like if (foo.isDefined) foo.get else MaybeULong.Nope
  // verbose but known-to-be-fast

  @inline def toMaybeJULong = if (isEmpty) MaybeJULong.Nope else new MaybeJULong(MaybeULong(__rep))
}

object MaybeULong {
  private val undefValue = -1L

  @inline final def apply(v: Long) = {
    Assert.usage(v >= 0)
    new MaybeULong(v)
  }

  val Nope = new MaybeULong(undefValue)
}

/**
 * Maybe[ULong] still isn't an AnyRef, We need an AnyRef class
 * that can be used to pass/return/store One(ULong) or Nope.
 *
 * Like a MaybeULong, but an AnyRef, so can be stored in
 * collections.
 *
 * This reduces boxing. You get one object with an unboxed MaybeULong
 * stored within it.
 */
final class MaybeJULong(mi: MaybeULong)
    extends Serializable {
  @inline final def get: Long = mi.get
  @inline final def getULong = mi.getULong
  //@inline final def getOrElse(alternate: Long): Long = mi.getOrElse(alternate)
  //@inline final def getULongOrElse(alternate: ULong): ULong = mi.getULongOrElse(alternate)
  @inline final def isDefined = mi.isDefined
  @inline final def isEmpty = !isDefined
  override def toString = mi.toString
}

object MaybeJULong {

  @inline def apply(v: Long) = new MaybeJULong(MaybeULong(v))

  val Nope = new MaybeJULong(MaybeULong.Nope)
}
