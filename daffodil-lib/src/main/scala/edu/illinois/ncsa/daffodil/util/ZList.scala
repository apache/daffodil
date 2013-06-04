package edu.illinois.ncsa.daffodil.util

/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG

/**
 * We need a list-like/stream-like thing, but that is
 * lazy in both the head and tail. The built in streams in
 * Scala are only tail-lazy.
 *
 * We call it ZList as in 'Z' as in being lazy, as in sleeping as in "ZZzzzzz..."
 */

object ZList {
  def uid = {
    val res = uid_
    uid_ += 1
    res
  }
  private var uid_ = 0L
}

abstract class ZList {
  def head: Any
  def tail: ZList
  protected var headVal = false
  protected var tailVal = false
  protected val uid = ZList.uid

  // print out no matter what goes wrong when evaluating.
  override def toString = {
    "Z@" + uid + "(" +
      (if (headVal)
        OOLAG.keepGoing("noValue") { head.toString }
      else "...") +
      ", " +
      (if (tailVal)
        OOLAG.keepGoing("noTail") { tail.toString }
      else "...") +
      ")"

  }
}
/**
 * lazy (sleeping) on both arguments hence double Z.
 */
class ZZ private (h: => Any, t: => ZList) extends ZList {
  lazy val head = { headVal = true; h }
  lazy val tail = { tailVal = true; t }
}

object ZZ {
  def apply(head: => Any, tail: => ZList) = new ZZ(head, tail)
}

object ZEnd extends ZList {
  lazy val head = Assert.usageError("head of ZEnd")
  lazy val tail = Assert.usageError("tail of ZEnd")
  override def toString = "ZEnd"
}