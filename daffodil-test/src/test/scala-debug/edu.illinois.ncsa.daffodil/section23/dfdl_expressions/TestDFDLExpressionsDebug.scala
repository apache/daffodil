package daffodil.section23.dfdl_expressions

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File
import daffodil.debugger.Debugger

class TestDFDLExpressionsDebug extends JUnitSuite {
  val testDir = "/daffodil/section23/dfdl_expressions/"
  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  // waiting for feature where string length can be given in units of bytes
  // even for variable-width character sets like utf-8 (which use 1 to 4 bytes per character)
  @Test def test_expressions_lke3_rel() { runner.runOneTest("lke3_rel") }

}
