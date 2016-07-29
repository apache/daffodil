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

import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * A pool is a collection of objects that are manually recycled usually via
 * some sort of a stack discipline.
 *
 * The point of it is to avoid excessive allocation of little objects.
 *
 * This is not thread safe.
 *
 * Either a pool becomes part of a state block that is separate per thread,
 * or it must be made using a ThreadLocal to get one per thread automatically.
 */
trait Pool[T <: AnyRef] {

  private val pool = new MStack.Of[T]

  private var numOutstanding: Int = 0

  protected def allocate: T

  final def getFromPool: T = {
    numOutstanding += 1
    if (pool.isEmpty) {
      allocate
    } else {
      pool.pop.asInstanceOf[T]
    }
  }

  final def returnToPool(thing: T) {
    numOutstanding -= 1
    pool.push(thing)
  }

  /**
   * Call this at end of execution to be sure all pooled items
   * have been returned.
   *
   * This is to help find resource leaks where items are taken from
   * the pool, but then dropped.
   */
  final def finalCheck {
    if (!(numOutstanding =#= 0)) {
      val msg = "Pool " + Misc.getNameFromClass(this) + " leaked " + numOutstanding + " instance(s)."
      Assert.invariantFailed(msg)
    }
  }

}
