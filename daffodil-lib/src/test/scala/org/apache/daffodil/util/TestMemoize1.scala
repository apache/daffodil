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

package org.apache.daffodil.util

import org.junit.Test
import org.junit.Assert._

class TestMemoize1 {

  @Test def testMemoize1() {
    var counter = 0

    def dontCallMeTwice(x: String): String = {
      counter = counter + 1
      x.map { x => x.toUpper }
    }

    val memoDontCallMeTwice = Memoize1(dontCallMeTwice)

    assertEquals("FOO", memoDontCallMeTwice("foo"))
    assertEquals("FOO", memoDontCallMeTwice("foo"))
    assertEquals("BAR", memoDontCallMeTwice("bar"))
    assertEquals("BAR", memoDontCallMeTwice("bar"))
    assertEquals("QUUX", memoDontCallMeTwice("quux"))
    assertEquals(3, counter)
  }

  /*
   * This next test illustrates that you can compose multiple uses of Memoize1 to get a
   *  multi-arg version of it.
   */

  /** So we can see how many times our function gets called. */
  var counter = 0

  /**
   * This is the function we wish to use Memoizing with.
   *
   * We rename it to put suffix "_notMemoized" and make private
   */
  private def threeArgFunctionExpensiveToCompute_notMemoized(x: String, y: String, z: String) = {
    counter = counter + 1
    ((x + y + z).toString, ()) // imagine this is expensive to compute, or we need exact eq object back each time
  }

  /**
   * Memoized version of the function. This is val because we want to
   * construct this thing exactly once.
   *
   * When this is applied, because of the way this is nested, no tuples should
   * have to be allocated per call. The allocating happens as the cache is
   * built.
   */
  private val memoDontCallMeTwiceSameArgs = {
    Memoize1((x: String) =>
      Memoize1((y: String) =>
        Memoize1((z: String) =>
          threeArgFunctionExpensiveToCompute_notMemoized(x, y, z))))
  }

  /**
   *  Here's our function that we want people to call/use, ignoring that
   *  memoizing is happening under the hood.
   */
  def threeArgFunction(x: String, y: String, z: String) = memoDontCallMeTwiceSameArgs(x)(y)(z)

  @Test def testMemoize3Arg(): Unit = {
    val res1 = threeArgFunction("foo", "bar", "baz")
    val res2 = threeArgFunction("quux", "flux", "pux")
    assertEquals(2, counter)
    assertEquals(("foobarbaz", ()), res1)
    assertEquals(("quuxfluxpux", ()), res2)
    val res3 = threeArgFunction("foo", "bar", "baz")
    val res4 = threeArgFunction("quux", "flux", "pux")
    assertEquals(2, counter) // no more calls happened.
    assertEquals(("foobarbaz", ()), res3)
    assertEquals(("quuxfluxpux", ()), res4)
    assertTrue(res1 eq res3) // results are EQ, not just equal.
    assertTrue(res2 eq res4)
  }

  /**
   * This test shows that memoized nests are efficient, i.e., calling them
   * isn't going to allocate tuples or closures or anything like that, because
   * the objects being returned are EQ - including the intermediate function
   * objects.
   *
   * These tests use Strings as the argument types, because if you used Int or
   * other primitive number types, then Boxed numbers are going to get allocated
   * for looking things up in the hash tables that Memoize1 is based on.
   *
   * So if you converted this into something for profiling, you'd see lots of allocating.
   * To avoid the allocation, always pass an AnyRef to this.
   */
  @Test def testMemoize3ArgNoAllocations(): Unit = {
    val m1 = memoDontCallMeTwiceSameArgs("first")
    val m2 = memoDontCallMeTwiceSameArgs("first")
    // m1 is a function object that is closed over the value "first" for x.
    assertTrue(m1 eq m2) // exact same object
    val m3 = m1("second")
    val m4 = m1("second")
    // m2 is a function object that is closed over the value "first" for x, and "second" for y.
    assertTrue(m3 eq m4)
    val m5 = m3("third")
    val m6 = m3("third")
    assertTrue(m5 eq m6)
    assertEquals(("firstsecondthird", ()), m5)
  }

  /**
   * This test (if you uncomment it), shows that passing a
   * primitive number type causes a scala compilation error.
   */
  //  @Test def testUsePrimitiveNumberArg(): Unit = {
  //
  //    val mf = Memoize1((x: Int) => x + x)
  //    val mf2 = Memoize1[Int, String]((x: Int) => x.toString)
  //
  //    mf(5)
  //    mf2(10)
  //  }

}
