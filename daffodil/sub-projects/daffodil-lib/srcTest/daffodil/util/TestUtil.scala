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
  
}