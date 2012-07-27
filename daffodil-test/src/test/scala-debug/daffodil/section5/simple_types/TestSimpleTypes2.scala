package daffodil.section5.simple_types

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestSimpleTypes2 extends JUnit3Suite {
  val testDir = "/daffodil/section5/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  def test_warning_exercise() { 
    val exc = intercept[Exception] {
    	runner.runOneTest("warning_exercise") }
    	assertTrue(exc.getMessage().contains("Did not find"))
  	}
  
  }
