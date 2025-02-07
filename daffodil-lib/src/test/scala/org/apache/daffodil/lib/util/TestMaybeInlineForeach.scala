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
  def usesForeach(m: Maybe[String]): Unit = {
    m._foreach { bar(_) }
  }

  /**
   * We want using foreach to behave exactly like this piece of code.
   */
  def usesIfDefined(m: Maybe[String]): Unit = {
    if (m.isDefined) bar(m.value)
  }

  /**
   * And that ifDefined test should behave exactly like this code.
   */
  def usesIfNull(m: String): Unit = {
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

  def testForeach(): Unit = {
    var i: Long = 0
    while (i < limit) {
      i += 1
      usesForeach(one)
    }
  }

  def testIfDefined(): Unit = {
    var i: Long = 0
    i = 0
    while (i < limit) {
      i += 1
      usesIfDefined(one)
    }
  }

  def testIfNull(): Unit = {
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
  def testForeachVersusIfDefined(): Unit = {
    val foreachNanos: Double = time(testForeach()).toDouble
    val ifDefinedNanos: Double = time(testIfDefined()).toDouble
    val ifNullNanos: Double = time(testIfNull()).toDouble
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
      fail(
        "foreach is taking less than 4x longer than if(x.isDefined)... " +
          "\nMaybe scala compiler is avoiding the function object now? (A good thing!)" +
          "\nratio ifDefined/foreach time is " + ratio
      )
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
      fail(
        "ifDefined is taking a lot longer than ifNull. They should be about the same. " +
          "\nratio ifNull/ifDefined is " + ratio2
      )
    } else {
      println("ratio ifNull/ifDefined is " + ratio2)
    }
  }

  def bar(s: String): Unit = {
    if (s == "won't equal this") println(s)
  }

}
