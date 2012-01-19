package daffodil.exceptions


import junit.framework.Assert._ ;
import org.scalatest.junit.JUnit3Suite ;


class TestExceptions extends JUnit3Suite {
  
   
  // @Test 
  def testAssert() {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }
   
  
}