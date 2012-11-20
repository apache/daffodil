package daffodil

import java.io.File
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.compiler.Compiler
import tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults
import daffodil.util.Misc
import org.junit.Test
import daffodil.debugger.Debugger

class TresysTests3 extends JUnitSuite {
  val testDir = "/test-suite/tresys-contributed/"

  val nsd = testDir + "nested-separator-delimited.tdml"
  lazy val runnerNSD = new DFDLTestSuite(Misc.getRequiredResource(nsd))

  @Test def test_optionalWithSeparators() { runnerNSD.runOneTest("optionalWithSeparators") }
}