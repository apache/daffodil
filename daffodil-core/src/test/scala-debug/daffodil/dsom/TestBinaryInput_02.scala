package daffodil.dsom

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.processors._
import daffodil.compiler._
import daffodil.debugger.Debugger

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_02 extends JUnitSuite {

  var runner = {
    val testDir = "/test-suite/tresys-contributed/"
    val aa = testDir + "BinaryInput_01.tdml"
    lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
    runner
  }

  /*** DFDL-379 US-ASCII-7-bit-packed text ***/

  @Test def test_packed7BitASCII3() = Debugger.withDebugger { runner.runOneTest("packed7BitASCII3") }
}
