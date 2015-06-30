package edu.illinois.ncsa.daffodil.section00.general

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger
import org.junit._

object TestElementFormDefaultGeneral {
  
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val aa = testDir + "testElementFormDefault.tdml"
  var runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  /**
   * Avoid memory leak of adding more and more test suites to static objects as we run more and more test suites.
   */
  @AfterClass def tearDown() { 
    runner = null 
  }

}

class TestElementFormDefaultGeneral {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val aa = testDir + "testElementFormDefault.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
 
  @Test def test_delimOptPresentQualified01() { runner.runOneTest("delimOptPresentQualified01") }
  @Test def test_delimOptPresentQualified02() { runner.runOneTest("delimOptPresentQualified02") }
  @Test def test_delimOptPresentQualified03() { runner.runOneTest("delimOptPresentQualified03") }
  @Test def test_delimOptPresentQualified04() { runner.runOneTest("delimOptPresentQualified04") }
  @Test def test_delimOptPresentQualified05() { runner.runOneTest("delimOptPresentQualified05") }

  @Test def test_delimOptPresentUnqualified01() { runner.runOneTest("delimOptPresentUnqualified01") }
  @Test def test_delimOptPresentUnqualified02() { runner.runOneTest("delimOptPresentUnqualified02") }
  @Test def test_delimOptPresentUnqualified03() { runner.runOneTest("delimOptPresentUnqualified03") }
  @Test def test_delimOptPresentUnqualified04() { runner.runOneTest("delimOptPresentUnqualified04") }
  @Test def test_delimOptPresentMissing() { runner.runOneTest("delimOptPresentMissing") }

  @Test def test_delimOptPresentGlobalQualified01() { runner.runOneTest("delimOptPresentGlobalQualified01") }
  @Test def test_delimOptPresentGlobalQualified02() { runner.runOneTest("delimOptPresentGlobalQualified02") }

}
