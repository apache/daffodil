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

package org.apache.daffodil.lib

import org.apache.daffodil.lib.Implicits._

import org.junit.Assert._
import org.junit.Test

/**
 * Scala Unit Testing Notes:
 *
 * It is important that the Eclipse IDE make it convenient to run the unit tests, step the user directly to the point
 * of failure, etc.
 *
 * Scalatest doesn't do this directly, but using it driven by JUnit does.
 *
 * So I'm advocating that a much more vanilla approach be taken to unit tests. Straight use of Junit.
 *
 * Here is an example. Some simple tests, some that intercept exceptions, and demonstrate that the intercept
 * device works properly.
 */
class HowToUseJUnit {

  @Test def test(): Unit = {
    assertEquals(42, 6 * 7)
    assertTrue(42 == 6 * 7)
    if (42 != 6 * 7)
      fail("wrong answer")
  }

  def somethingThatThrows(): Unit = {
    throw new NumberFormatException()
  }

  @Test def testOfInterceptToTestExpectedThrows(): Unit = {
    intercept[NumberFormatException] {
      // println("here we are")
      somethingThatThrows()
    }
  }

  // @Test
  def testOfInterceptReturnedValue(): Unit = {
    val nfe = intercept[NumberFormatException] {
      // println("here we are")
      somethingThatThrows()
    }
    if (!nfe.getClass().getName().contains("NumberFormatException"))
      fail("object returned from intercept not the right one.")
  }

  // @Test
  //  def testOfInterceptToTestExpectedThrowsButItThrewSomethingElse() {
  //   val e = intercept[JUnitTestFailedError] { // FIXME: Not right exception to catch...
  //      intercept[NumberFormatException] { // won't get this one, so it will throw
  //        //println("there we go")
  //        throw new Exception("foobar")
  //      }
  //    }
  //   if (!e.getMessage().contains("foobar"))
  //     fail("didn't propagate unintercepted throw properly.")
  //  }

  // @Test
  //  def testOfInterceptToTestExpectedThrowsButItDidntThrowAtAll() {
  //    try {
  //      intercept[NumberFormatException] {
  //        // don't throw anything.
  //        // println("not going to throw")
  //      }
  //    } catch {
  //      case e: JUnitTestFailedError => // we're good // is this the wrong exception to catch
  //      case e => throw e
  //    }
  //  }

}
