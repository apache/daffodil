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

import org.junit.Assert._

final class TestMaybeInlineForeach {

  def time[R](body: => Unit) = {
    val t0 = System.nanoTime()
    body
    val t1 = System.nanoTime()
    // println("Time taken: " + (t1 - t0) + "ns")
    (t1 - t0)
  }

  var foobar = "foobar"
  var one = Maybe(foobar)

  /**
   * Disassemble to see that scala/java byte code does/does-not have a function object involved.
   */
  def usesForeach(m: Maybe[String]) {
    m._foreach { bar(_) }
  }

  /**
   * We want using foreach to behave exactly like this piece of code.
   */
  def usesIfDefined(m: Maybe[String]) {
    if (m.isDefined) bar(m.value)
  }

  /**
   * And that ifDefined test should behave exactly like this code.
   */
  def usesIfNull(m: String) {
    if (m != null) bar(m)
  }
  /**
   * Call these enough times that the hotspot optimizer should be inlining
   * whatever it can.
   *
   * The point is to give scala and the jvm every opportunity to optimize out
   * the allocation of the closure object being passed to Maybe.foreach in test1,
   * such that test1 above and test2 behave identically.
   *
   * If the tests that avoid _foreach are always superior, then we should drop the foreach and related
   * methods from Maybe that take function object arguments.
   */
  @inline private def limit = 1000000000L // 50000000000L runs for about a minute

  def testForeach {
    var i: Long = 0
    while (i < limit) {
      i += 1
      usesForeach(one)
    }
  }

  def testIfDefined {
    var i: Long = 0
    i = 0
    while (i < limit) {
      i += 1
      usesIfDefined(one)
    }
  }

  def testIfNull {
    var i: Long = 0
    i = 0
    while (i < limit) {
      i += 1
      usesIfNull(foobar)
    }
  }

  /**
   * Commented out because turns out that when we run this as part of normal regression
   * it fails sometimes. It is reliable if you make the number of iterations larger,
   * but there's no point waiting 2 seconds for this test to run all the time.
   */
  // @Test
  def testForeachVersusIfDefined {
    val foreachNanos: Double = time(testForeach)
    val ifDefinedNanos: Double = time(testIfDefined)
    val ifNullNanos: Double = time(testIfNull)
    //
    // if foreach is taking more than 4x the time of ifDefined, that's
    // what we expect if scala can't inline-away the function object allocation

    // This will fail if foreach takes less than that much, because that
    // is telling us the scala compiler is either more aggressively inlining 
    // the functions (so no function being allocated), or it's somehow screwing up
    // if(x.isDefined)... 
    // 
    // Either way we have to investigate.
    //
    val ratio = ifDefinedNanos / foreachNanos
    if (ratio > 0.25) {
      fail("foreach is taking less than 4x longer than if(x.isDefined)... " +
        "\nMaybe scala compiler is avoiding the function object now? (A good thing!)" +
        "\nratio ifDefined/foreach time is " + ratio)
    } else {
      println("ratio ifDefined/foreach is " + ratio)
    }
    //
    // Tests that if (x.isDefined) is within 2x speed of if (x eq null)
    //
    // If that's not the case, then we need to investigate, because it should be 
    // roughly as fast.
    val ratio2 = ifNullNanos / ifDefinedNanos
    if (ratio2 < 0.8) {
      fail("ifDefined is taking a lot longer than ifNull. They should be about the same. " +
        "\nratio ifNull/ifDefined is " + ratio2)
    } else {
      println("ratio ifNull/ifDefined is " + ratio2)
    }
  }

  def bar(s: String) {
    if (s == "won't equal this") println(s)
  }

}
