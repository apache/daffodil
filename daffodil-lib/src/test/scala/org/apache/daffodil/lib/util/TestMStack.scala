/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.util

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
