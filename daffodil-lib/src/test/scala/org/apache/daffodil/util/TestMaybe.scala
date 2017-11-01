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


class TestMaybe {

  type JINT = java.lang.Long
  var e: JINT = 5

  var t: Long = 0

  def passOne(mi: Maybe[JINT]) = {
    if (mi.isDefined) t += mi.get
  }

  def returnOne(e: JINT) = Maybe(e)

  /**
   * This test can be used to verify that passing Maybe[T] objects
   * and returning Maybe[T] objects doesn't cause allocation to occur.
   *
   * Uncomment and run, then examine with jvisualvm or jprofiler or some
   * tool where you can watch allocation on the main thread.
   */
  //  @Test def testIfMaybeAllocatesOnPassOrReturn {
  //    var i: Int = 0
  //    while (i < 10000000000000L) {
  //      //
  //      // While this is running, watch on jvisualvm or jprofiler
  //      // to see if allocation is occurring on the main thread.
  //      //
  //      // slow allocation on other threads is to be expected.
  //      //
  //      i += 1
  //      passOne(Maybe(e))
  //      val mi = returnOne(e)
  //      if (mi.isDefined) t += mi.get
  //    }
  //    println(t)
  //  }

}
