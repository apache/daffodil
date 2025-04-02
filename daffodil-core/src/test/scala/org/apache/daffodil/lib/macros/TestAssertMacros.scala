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

package org.apache.daffodil.lib.macros

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions._

import org.junit.Assert._
import org.junit.Test

class TestAssertMacros {

  var x = 0

  @Test def testInvariant1Arg(): Unit = {
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

  @Test def testUsage1Arg(): Unit = {
    val e = intercept[UsageException] {
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

  @Test def testUsage2Arg(): Unit = {
    val e = intercept[UsageException] {
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
  @Test def testMacrosNotEvaluatedSecondArg(): Unit = {
    Assert.usage(true, { fail(); "failed" })
  }

  @Test def testNotYetImplemented0Arg(): Unit = {
    val e = intercept[NotYetImplementedException] {
      Assert.notYetImplemented()
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Not yet implemented"))
  }

  @Test def testNotYetImplemented1Arg(): Unit = {
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

  @Test def testNotYetImplemented2Arg(): Unit = {
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

  @Test def testUsage2ArgCause(): Unit = {
    val e = intercept[UsageException] {
      Assert.usageWithCause(x == 1, new Exception("test"))
    }
    val cause = e.getCause.toString
    assertTrue(cause.contains("Exception"))
    assertTrue(cause.contains("test"))
  }
}
