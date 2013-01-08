package daffodil.section08.property_scoping

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

class TestPropertyScoping extends JUnitSuite {
  val testDir = "/daffodil/section08/property_scoping/"
  val aa = testDir + "PropertyScoping.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_property_scoping_01() { runner.runOneTest("property_scoping_01") }
  @Test def test_property_scoping_06() { runner.runOneTest("property_scoping_06") }
  @Test def test_property_group_ref() { runner.runOneTest("group_ref") }
  @Test def test_property_shortFormSchemaFail() { runner.runOneTest("shortFormSchemaFail") }

  val tdml = testDir + "PropertyScoping_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_property_scoping_02() { runner_01.runOneTest("property_scoping_02") }
  //  @Test def test_property_scoping_03() = Debugger.withDebugger {
  //    LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  //    runner_01.runOneTest("property_scoping_03")
  //  }
  @Test def test_property_scoping_04() { runner_01.runOneTest("property_scoping_04") }
  @Test def test_property_scoping_05() { runner_01.runOneTest("property_scoping_05") }
  @Test def testNearestEnclosingSequenceElementRef() { runner_01.runOneTest("NearestEnclosingSequenceElementRef") }
}
