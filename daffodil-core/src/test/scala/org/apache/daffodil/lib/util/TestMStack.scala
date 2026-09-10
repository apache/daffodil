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

import org.apache.daffodil.lib.util.Maybe.*

import org.junit.Assert.*
import org.junit.Test

/**
 * Compare MStack performance to ArrayStack. It should be faster for primitives
 */
class TestMStack {

  var junk: Long = 0

  /**
   * A destination sized to a source's actual depth, then copyFrom'd, must
   * end up identical to one constructed without specifying a size.
   */
  @Test def testMStackOfCopyFromSizedDestinationMatchesDefault(): Unit = {
    val source = new MStackOf[String]
    source.push("a")
    source.push("b")
    source.push("c")

    val sizedDest = new MStackOf[String](source.length)
    sizedDest.copyFrom(source)

    val defaultDest = new MStackOf[String]
    defaultDest.copyFrom(source)

    assertEquals(defaultDest.toList, sizedDest.toList)
    assertEquals(3, sizedDest.length)
    assertEquals("c", sizedDest.top)
  }

  /**
   * A sized-to-exact-depth destination must still grow correctly if
   * something pushes beyond its initial capacity.
   */
  @Test def testMStackOfSizedDestinationStillGrowsCorrectly(): Unit = {
    val source = new MStackOf[String]
    source.push("a")
    source.push("b")

    val sizedDest = new MStackOf[String](source.length)
    sizedDest.copyFrom(source)

    sizedDest.push("c")
    sizedDest.push("d")
    sizedDest.push("e")

    assertEquals(5, sizedDest.length)
    assertEquals("e", sizedDest.top)
    assertEquals(List("e", "d", "c", "b", "a"), sizedDest.toList)
  }

  /** Same sized-clone pattern, but for MStackOfMaybe. */
  @Test def testMStackOfMaybeCopyFromSizedDestinationMatchesDefault(): Unit = {
    val source = new MStackOfMaybe[String]
    source.push(One("x"))
    source.push(Nope)
    source.push(One("z"))

    val sizedDest = new MStackOfMaybe[String](source.length)
    sizedDest.copyFrom(source)

    val defaultDest = new MStackOfMaybe[String]
    defaultDest.copyFrom(source)

    assertEquals(defaultDest.toListMaybe, sizedDest.toListMaybe)
    assertEquals(3, sizedDest.length)
    assertEquals(One("z"), sizedDest.top)
    assertEquals(One("z"), sizedDest.pop)
    assertEquals(Nope, sizedDest.pop)
    assertEquals(One("x"), sizedDest.pop)
    assertTrue(sizedDest.isEmpty)
  }

  /** Same sized-construction pattern, for the primitive-specialized variants. */
  @Test def testMStackOfBooleanSizedConstructionWorks(): Unit = {
    val stk = MStackOfBoolean(3)
    stk.push(true)
    stk.push(false)
    stk.push(true)
    assertEquals(3, stk.length)
    assertEquals(true, stk.pop())
    assertEquals(false, stk.pop())
    assertEquals(true, stk.pop())
  }

  @Test def testMStackOfIntSizedConstructionWorks(): Unit = {
    val stk = MStackOfInt(2)
    stk.push(1)
    stk.push(2)
    stk.push(3)
    assertEquals(3, stk.length)
    assertEquals(3, stk.pop())
    assertEquals(2, stk.pop())
    assertEquals(1, stk.pop())
  }

  @Test def testMStackOfLongSizedConstructionWorks(): Unit = {
    val stk = MStackOfLong(1)
    stk.push(10L)
    stk.push(20L)
    assertEquals(2, stk.length)
    assertEquals(20L, stk.pop())
    assertEquals(10L, stk.pop())
  }

  /**
   * maxSizeReached is diagnostic only (for deciding whether a type's
   * default initialSize is well-chosen), but it must actually track the
   * high-water mark across pushes/pops, not just the current length.
   */
  @Test def testMaxSizeReachedTracksHighWaterMark(): Unit = {
    val prior = MStack.trackMaxSizeReached
    MStack.trackMaxSizeReached = true
    try {
      val stk = new MStackOf[String]
      assertEquals(0, stk.maxSizeReached)

      stk.push("a")
      stk.push("b")
      stk.push("c")
      assertEquals(3, stk.maxSizeReached)

      stk.pop
      stk.pop
      assertEquals(1, stk.length)
      assertEquals(
        "popping must not reduce maxSizeReached - it's a high-water mark, not the current length",
        3,
        stk.maxSizeReached
      )

      stk.push("d")
      assertEquals(3, stk.maxSizeReached)
    } finally {
      MStack.trackMaxSizeReached = prior
    }
  }

  /** copyFrom must preserve the source's high-water mark, not just its current contents. */
  @Test def testMaxSizeReachedPropagatesThroughCopyFrom(): Unit = {
    val prior = MStack.trackMaxSizeReached
    MStack.trackMaxSizeReached = true
    try {
      val source = new MStackOf[String]
      source.push("a")
      source.push("b")
      source.push("c")
      source.pop
      assertEquals(2, source.length)
      assertEquals(3, source.maxSizeReached)

      val dest = new MStackOf[String]
      dest.copyFrom(source)
      assertEquals(2, dest.length)
      assertEquals(
        "expected copyFrom to carry over the source's high-water mark, not just its current contents",
        3,
        dest.maxSizeReached
      )
    } finally {
      MStack.trackMaxSizeReached = prior
    }
  }

  /** Off by default: pushes must not update maxSizeReached unless tracking is explicitly enabled. */
  @Test def testMaxSizeReachedDisabledByDefault(): Unit = {
    assertEquals(false, MStack.trackMaxSizeReached)
    val stk = new MStackOf[String]
    stk.push("a")
    stk.push("b")
    stk.push("c")
    assertEquals(0, stk.maxSizeReached)
  }

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
