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

package edu.illinois.ncsa.daffodil

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

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

  @Test def test() {
    assertEquals(42, 6 * 7)
    assertTrue(42 == 6 * 7)
    if (42 != 6 * 7)
      fail("wrong answer")
  }

  def somethingThatThrows() {
    throw new NumberFormatException()
  }

  @Test def testOfInterceptToTestExpectedThrows() {
    intercept[NumberFormatException] {
      //println("here we are")
      somethingThatThrows()
    }
  }

  // @Test
  def testOfInterceptReturnedValue() {
    val nfe = intercept[NumberFormatException] {
      //println("here we are")
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
  //      case e: JUnitTestFailedError => // we're good // FIXME: wrong exception to catch
  //      case e => throw e
  //    }
  //  }

}
