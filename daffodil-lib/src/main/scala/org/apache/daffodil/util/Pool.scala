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

import org.apache.daffodil.equality._
import org.apache.daffodil.exceptions.Assert
import scala.collection.mutable

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

/**
 *  Derive from this for debug purposes if you want to pool things.
 */
trait Poolable {
  /**
   * The debug label is used to report diagnostics about pool management errors.
   * It helps to pinpoint the problematic place where pooled items are being
   * mismanaged. You end up passing say, Misc.getNameFromClass() to this.
   */
  private var poolDebugLabel_ : String = null
  final def setPoolDebugLabel(debugLabel: String): Unit = { poolDebugLabel_ = debugLabel }
  final def poolDebugLabel: String = poolDebugLabel_
}

trait Pool[T <: Poolable] {

  private val pool = new MStackOf[T]

  /**
   * Keeps track of pool objects that have not been freed for debug
   * purposes.
   */
  private val inUse = new mutable.HashSet[T]

  private var numOutstanding: Int = 0

  protected def allocate: T

  final def getFromPool(debugLabel: String): T = {
    numOutstanding += 1
    val instance =
      if (pool.isEmpty) {
        allocate
      } else {
        pool.pop.asInstanceOf[T]
      }
    instance.setPoolDebugLabel(debugLabel)
    inUse += instance
    instance
  }

  final def returnToPool(thing: T) {
    numOutstanding -= 1
    pool.push(thing)
    inUse -= thing
    thing.setPoolDebugLabel(null)
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
      val msg = "Pool " + Misc.getNameFromClass(this) + " leaked " + numOutstanding + " instance(s)." +
        "\n" + inUse.map { item => "poolDebugLabel = " + item.poolDebugLabel }.mkString("\n")
      Assert.invariantFailed(msg)
    }
  }

}
