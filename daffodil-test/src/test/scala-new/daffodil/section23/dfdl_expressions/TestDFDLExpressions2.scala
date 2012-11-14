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

class TestDFDLExpressions2 extends JUnitSuite {
  val testDir = "/daffodil/section23/dfdl_expressions/"
  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  // ArrayOptElem_02 should pass when binary unsignedInt is implemented.
  //  @Test def test_ArrayOptElem_02() { runner.runOneTest("ArrayOptElem_02") }
  //  @Test def test_expressions_lke3_rel() { runner.runOneTest("lke3_rel") }
  //  @Test def test_ocke_rel() { runner.runOneTest("ocke_rel") }
  @Test def test_expresion_bad_path_to_variable() { runner.runOneTest("expresion_bad_path_to_variable") }
  @Test def test_checkConstraints() { runner.runOneTest("dfdlCheckConstraints") }
  @Test def test_maxOccursPass() { runner.runOneTest("checkMaxOccurs_Pass") }
  @Test def test_maxOccursUnboundedPass() { runner.runOneTest("checkMaxOccursUnbounded_Pass") }

  @Test def test_repeatFlags1() { runner.runOneTest("repeatFlags1") }
  @Test def test_repeatFlags2() = { runner.runOneTest("repeatFlags2") }
  @Test def test_repeatFlags3() { runner.runOneTest("repeatFlags3") }
}
