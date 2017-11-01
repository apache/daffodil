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

package org.apache.daffodil.macros

import org.apache.daffodil.exceptions._
import org.apache.daffodil.Implicits._

import org.junit.Test
import org.junit.Assert._

class TestAssertMacros {

  var x = 0

  @Test def testInvariant1Arg() {
    val e = intercept[Abort] {
      Assert.invariant(if (1 == x) true else false)
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Invariant broken"))
    assertTrue(msg.contains("if (1"))
    assertTrue(msg.contains("x))"))
    assertTrue(msg.contains("true"))
    assertTrue(msg.contains("else"))
    assertTrue(msg.contains("false"))
  }

  @Test def testUsage1Arg() {
    val e = intercept[Abort] {
      Assert.usage(if (1 == x) true else false)
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Usage error"))
    assertTrue(msg.contains("if (1"))
    assertTrue(msg.contains("x))"))
    assertTrue(msg.contains("true"))
    assertTrue(msg.contains("else"))
    assertTrue(msg.contains("false"))
  }

  @Test def testUsage2Arg() {
    val e = intercept[Abort] {
      Assert.usage(if (1 == x) true else false, "foobar")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Usage error"))
    assertTrue(msg.contains("if (1"))
    assertTrue(msg.contains("x))"))
    assertTrue(msg.contains("true"))
    assertTrue(msg.contains("else"))
    assertTrue(msg.contains("false"))
    assertTrue(msg.contains("foobar"))
  }

  /**
   * This test demonstrates that if the test holds, then
   * the 2nd argument is never evaluated at all and so can
   * be something expensive that computes a message string.
   */
  @Test def testMacrosNotEvaluatedSecondArg() {
    Assert.usage(if (1 == x) true else true, { fail(); "failed" })
  }

  @Test def testNotYetImplemented0Arg() {
    val e = intercept[NotYetImplementedException] {
      Assert.notYetImplemented()
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Not yet implemented"))
  }

  @Test def testNotYetImplemented1Arg() {
    val e = intercept[NotYetImplementedException] {
      Assert.notYetImplemented(if (0 == x) true else false)
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Not yet implemented"))
    assertTrue(msg.contains("if (0"))
    assertTrue(msg.contains("x))"))
    assertTrue(msg.contains("true"))
    assertTrue(msg.contains("else"))
    assertTrue(msg.contains("false"))
  }

  @Test def testNotYetImplemented2Arg() {
    val e = intercept[NotYetImplementedException] {
      Assert.notYetImplemented(if (0 == x) true else false, "foobar")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Not yet implemented"))
    assertTrue(msg.contains("if (0"))
    assertTrue(msg.contains("x))"))
    assertTrue(msg.contains("true"))
    assertTrue(msg.contains("else"))
    assertTrue(msg.contains("false"))
    assertTrue(msg.contains("foobar"))
  }
}
