package edu.illinois.ncsa.daffodil.equality

import org.junit.Test
import org.junit.Assert._

class TestEqualityOperators {

  @Test
  def testConveribleNumberEquality() {
    val x = 5
    val y = 6L
    assertFalse(x =#= y)
    // assertFalse (x =#= "x")  // compile error - wrong types
    assertFalse(5 =#= 6.0)
  }

  @Test
  def testStronglyTypedEquality() {
    val x = List(1, 2, 3)
    val y = Seq(1, 2, 3)
    assertTrue(x =:= y) // allowed since they're subtypes
    // assertFalse(x =:= "List(1, 2, 3") // compile error
    assertFalse(Nil =:= x)
  }
}