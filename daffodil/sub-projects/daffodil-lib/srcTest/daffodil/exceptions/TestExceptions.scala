package daffodil.exceptions
import org.scalatest.junit.JUnitSuite
import org.junit.Test

class TestExceptions extends JUnitSuite {
  
  @Test 
  def testAssert() {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }
  
}