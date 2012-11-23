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

  // ArrayOptElem_02 should pass when binary unsignedInt is implemented.
  @Test def test_ArrayOptElem_02() { runner.runOneTest("ArrayOptElem_02") }
  @Test def test_expressions_lke3_rel() { runner.runOneTest("lke3_rel") }
  @Test def test_ocke_rel() { runner.runOneTest("ocke_rel") }

  // Once bit-alignment and bit lengthUnits are implemented these should work.
  @Test def test_bit1() = Debugger.withDebugger { runner.runOneTest("bit1") }

  @Test def test_repeatBitFlags1() { runner.runOneTest("repeatBitFlags1") }
  @Test def test_repeatBitFlags2() = Debugger.withDebugger { runner.runOneTest("repeatBitFlags2") }
  @Test def test_repeatBitFlags3() { runner.runOneTest("repeatBitFlags3") }
}
