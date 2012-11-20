package daffodil.section12.delimiter_properties

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

class TestDelimiterProperties_01 extends JUnitSuite {

  val testDir_02 = "/daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  lazy val r = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  @Test def test_E1() = Debugger.withDebugger {
    LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    r.runOneTest("E1")
  }

}
