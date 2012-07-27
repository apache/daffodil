package daffodil.section7.defineFormat

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class defineFormatTests extends JUnit3Suite {
  val testDir = "/daffodil/section7/defineFormat/"
  val tdml = testDir + "defineFormat.tdml"
  val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  def test_defineFormat_01() { runner.runOneTest("defineFormat_01") }
  
  }
