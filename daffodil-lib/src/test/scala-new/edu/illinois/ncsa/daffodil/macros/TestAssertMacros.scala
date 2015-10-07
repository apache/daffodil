package edu.illinois.ncsa.daffodil.macros

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.Implicits._

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
