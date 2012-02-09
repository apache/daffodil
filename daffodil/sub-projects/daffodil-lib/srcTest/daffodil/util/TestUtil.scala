package daffodil.util

import junit.framework.Assert._ ;
import org.scalatest.junit.JUnit3Suite ;
import daffodil.exceptions._

class TestUtil extends JUnit3Suite {
  
//  @Test 
  def testStripQuotes() {
    assertEquals("foo", Misc.stripQuotes("\"foo\""))
  }
  
//  @Test 
  def testAssert() {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }
  
//  @Test 
  def testCopyrightNotices() {
    val fake = """
  Test fake Copyright (C) 3023 by Space Invaders of the Earth. All rights reserved.
  Of course these can be long verbose license statements and such.
  """ 
    CopyrightNotices.add(fake)
    val notices = CopyrightNotices.getNotices()
    val hasFake = notices.contains("Test fake Copyright")
    assertTrue(hasFake)
  }
}