/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.testEquality

import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.equality._

class TestEqualityOperators {

  @Test
  def testConveribleNumberEquality() {
    val x = 5
    val y = 6L
    assertFalse(x.toLong =#= y)
    // assertFalse (x =#= "x")  // compile error - wrong types
    assertFalse(5 =#= 6.0.toInt)
  }

  @Test
  def testStronglyTypedEquality() {
    val x = List(1, 2, 3)
    val y = Seq(1, 2, 3)
    assertTrue(x =:= y) // allowed since they're subtypes
    // assertFalse(x =:= "List(1, 2, 3") // compile error
    assertFalse(Nil =:= x)
  }

  // prevent optimizations from using constant objects
  val xObj = if (scala.math.random == -0.0) "foo" else "bar"
  val yObj = if (scala.math.random == -0.0) "bar" else "foo"

  @Test
  def testStronglyTypedEqualityInlineAnyRef() {
    if (TestEqualityOperators.compare(xObj, yObj))
      fail("equal")
  }

  private val ylong = scala.math.random.toLong

  @Test
  def testStronglyTypedEqualityInline() {
    val x = 5
    val y = ylong
    if (TestEqualityOperators.compareIntLong(x, y))
      fail("equal")
  }
}

/**
 * By looking at the byte code for the methods of this
 * object, one can determine whether these typed equality operators
 * are allocating or not, and what code is generated.
 */
object TestEqualityOperators {

  def compare(x: String, y: String) = {
    x =:= y
  }

  def compareIntLong(x: Int, y: Long) = {
    x.toLong =#= y
  }
}
