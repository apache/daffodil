package daffodil.section12.lengthKind

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
import daffodil.debugger.Debugger.withDebugger
import daffodil.debugger.Debugger

class TestLengthKindExplicitNew extends JUnitSuite {
  val testDir = "/daffodil/section12/lengthKind/"
  val aa = testDir + "ExplicitTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger { 
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name") 
  // }

  @Test def test_ExplicitLengthBitsNotFixed() = { runner.runOneTest("test_ExplicitLengthBitsNotFixed") }
  @Test def test_ExplicitLengthBitsFixed() = { runner.runOneTest("test_ExplicitLengthBitsFixed") }
  @Test def test_ExplicitLengthCharsNotFixed() = { runner.runOneTest("test_ExplicitLengthCharsNotFixed") }
  @Test def test_ExplicitLengthCharsFixed() = { runner.runOneTest("test_ExplicitLengthCharsFixed") }
  
}
