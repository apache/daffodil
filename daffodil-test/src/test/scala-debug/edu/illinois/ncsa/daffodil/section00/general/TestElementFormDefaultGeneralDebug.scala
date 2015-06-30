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

object TestElementFormDefaultGeneralDebug {
  
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

class TestElementFormDefaultGeneralDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val aa = testDir + "testElementFormDefault.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
}
