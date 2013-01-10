package daffodil.section06.entities

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

class TestEntities_01 extends JUnitSuite {

  val testDir_01 = "/daffodil/section06/entities/"
  val aa_01 = testDir_01 + "Entities.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(aa_01))

  //@Test def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  @Test def test_byte_entities_6_06() { runner_01.runOneTest("byte_entities_6_06") }
  @Test def test_byte_entities_6_07() { runner_01.runOneTest("byte_entities_6_07") }

  val testDir_02 = "/daffodil/ibm-tests/"
  val tdml_02 = testDir_02 + "dpaext1.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  // Needs dfdl:utf16Width='variable' implementation
  @Test def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }
  
  @Test def test_whitespace_01() { runner_01.runOneTest("whitespace_01") }

}
