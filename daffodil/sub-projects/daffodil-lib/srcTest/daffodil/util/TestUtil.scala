package daffodil.util

import junit.framework.Assert._
import daffodil.exceptions._
import org.scalatest.junit.JUnitSuite
import org.junit.Test

class TestUtil extends JUnitSuite {
  
  @Test def testGetRequiredResourceSucceeds () {
    val res = Misc.getRequiredResource("/xsd/XMLSchema.xsd")
    assertNotNull(res)
  }
  
  @Test def testGetRequiredResourceFails () {
    val e = intercept[Exception] {
    	val res = Misc.getRequiredResource("/xsd/NotAResourceName.foo")
    }
    assertTrue(e.getMessage().contains("NotAResourceName"))
  }
  
  @Test
  def testStripQuotes() {
    assertEquals("foo", Misc.stripQuotes("\"foo\""))
  }
  
  @Test 
  def testAssert() {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }
  
  @Test 
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
