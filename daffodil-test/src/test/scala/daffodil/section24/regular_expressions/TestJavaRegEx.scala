package daffodil.section24.regular_expressions

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import java.util.regex._

class TestJavaRegEx extends JUnitSuite {
  
	@Test def testRegEx_01() {
		val p = Pattern.compile("([A-Za-z]{1,8}(-[A-Za-z0-9]{1,8})*)");
	    val m = p.matcher("en-US");
	    assertTrue(m.matches());
	}
	
	@Test def testRegEx_02() {
		val p = Pattern.compile("([A-Za-z]{1,8}([-][A-Za-z0-9]{1,8})*)");
	    val m = p.matcher("en-US");
	    assertTrue(m.matches());
	}
}