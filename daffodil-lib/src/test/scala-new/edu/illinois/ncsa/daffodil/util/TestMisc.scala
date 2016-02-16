package edu.illinois.ncsa.daffodil.util

import org.junit.Test
import org.junit.Assert._

class TestMisc {

  @Test def testIsAllUpper() {
    assertTrue(Misc.isAllUpper("A", 0))
    assertFalse(Misc.isAllUpper("a", 0))
    assertTrue(Misc.isAllUpper("AB", 0))
    assertFalse(Misc.isAllUpper("Ab", 0))
    assertTrue(Misc.isAllUpper("ABC", 0))
    assertFalse(Misc.isAllUpper("ABc", 0))

    assertTrue(Misc.isAllUpper("AB", 1))
    assertFalse(Misc.isAllUpper("Ab", 1))
    assertTrue(Misc.isAllUpper("ABC", 1))
    assertFalse(Misc.isAllUpper("ABc", 1))
  }

  @Test def testToInitialLowerUnlessAllUpper() {
    assertEquals("fooBar", Misc.toInitialLowerCaseUnlessAllUpperCase("FooBar"))
    assertEquals("FOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("FOOBAR"))
    assertEquals("fOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("fOOBAR"))
    assertEquals("foobar", Misc.toInitialLowerCaseUnlessAllUpperCase("Foobar"))
  }
}
