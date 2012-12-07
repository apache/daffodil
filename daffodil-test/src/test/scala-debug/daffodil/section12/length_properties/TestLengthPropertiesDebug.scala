package daffodil.section12.length_properties

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

class TestLengthPropertiesDebug extends JUnitSuite {
  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  //@Test def test_length_explicit_12_01() { runner_01.runOneTest("length_explicit_12_01") }
  //@Test def test_length_explicit_12_02() { runner_01.runOneTest("length_explicit_12_02") }

  val testDir_02 = "/daffodil/section12/length_properties/"
  val tdml_02 = testDir_02 + "LengthProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  // should just skip the 2 excess bytes
  @Test def test_LengthProp_06() { runner_02.runOneTest("LengthProp_06") }

  // waiting for lengthUnits characters to be implemented.
  @Test def test_LengthProp_lengthExpression() { runner_02.runOneTest("LengthProp_lengthExpression1") }
  
  @Test def test_bitShort() { runner_02.runOneTest("bitShort") }
  
  @Test def test_LengthProp_bits_bool() { runner_02.runOneTest("LengthProp_bits_bool") }

}
