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

/**
 * Compare MStack performance to ArrayStack. It should be faster for primitives
 */
class TestMStack {

  var junk: Long = 0

  /**
   * This test compares MStackOfLong to ArrayStack[Long].
   *
   * It shows MStackOfLong is faster, this is probably due to the
   * fact that it can avoid boxing/unboxing the numbers. However, I haven't
   * disassembled the byte code to verify that this is the difference.
   *
   * (On my laptop: 64%)
   */
  //  @Test def testMStackSpeed1 {
  //    val numOps: Long = 1000000000
  //    val longStack = MStackOfLong
  //    val arrayStack = new ArrayStack[Long]
  //
  //    // Warm up the JVM on MStack
  //    var i: Long = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      longStack.push(0L)
  //      longStack.pop
  //    }
  //    // Warm up JVM on ArrayStack
  //    i = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      arrayStack.push(0L)
  //      arrayStack.pop
  //    }
  //    val (nanosMStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        longStack.push(0L)
  //        longStack.pop
  //      }
  //
  //    }
  //
  //    val (nanosAStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        arrayStack.push(0L)
  //        arrayStack.pop
  //      }
  //    }
  //
  //    val result = (nanosMStack * 100.0) / nanosAStack.toDouble
  //
  //    println("MStack runs in " + result + "% of the time of ArrayStack.")
  //
  //  }
  //
  //  /**
  //   * Compare performance for MStackOfMaybe[Thing] vs ArrayStack[Maybe[Thing]]
  //   *
  //   * Faster, probably due to lack of need to box/unbox the Maybe[Thing].
  //   *
  //   * (On my laptop: 17% of the time.)
  //   */
  //  @Test def testMStackMaybeSpeed2 {
  //    val numOps: Long = 100000000
  //    object Thing extends Thing
  //    class Thing {
  //      val n = 1
  //    }
  //    val mThing = Maybe(Thing)
  //    val mThingStack = new MStackOf[Thing]
  //    val arrayStack = new ArrayStack[Maybe[Thing]]
  //
  //    // Warm up the JVM on MStack
  //    var i: Long = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      mThingStack.push(mThing.get)
  //      val popped = Maybe(mThingStack.pop)
  //      junk += popped.get.n
  //    }
  //    // Warm up JVM on ArrayStack
  //    i = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      arrayStack.push(One(Thing))
  //      junk += arrayStack.pop.get.n
  //    }
  //    val (nanosMStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        mThingStack.push(mThing.get)
  //        val popped = Maybe(mThingStack.pop)
  //        junk += popped.get.n
  //      }
  //
  //    }
  //
  //    val (nanosAStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        arrayStack.push(One(Thing))
  //        junk += arrayStack.pop.get.n
  //      }
  //    }
  //
  //    val result = (nanosMStack * 100.0) / nanosAStack.toDouble
  //
  //    println("MStack runs in " + result + "% of the time of ArrayStack.") // 63% under eclipse, no optimizers.
  //
  //  }

}
