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

class TestEntities extends JUnitSuite {
  val testDir = "/daffodil/section06/entities/"
  val tdml = testDir + "charClassEntities.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  @Test def test_LineFeed() { runner.runOneTest("LineFeed") }
  @Test def test_CarriageReturn() { runner.runOneTest("CarriageReturn") }
  @Test def test_LineSeparator() { runner.runOneTest("LineSeparator") }
  @Test def test_NextLine() { runner.runOneTest("NextLine") }
  @Test def test_LineFeed_byte() { runner.runOneTest("LineFeed_byte") }
  @Test def test_CarriageReturn_byte() { runner.runOneTest("CarriageReturn_byte") }
  @Test def test_CRLF_byte() { runner.runOneTest("CRLF_byte") }
  @Test def test_LineSeparator_byte() { runner.runOneTest("LineSeparator_byte") }
  @Test def test_NextLine_byte() { runner.runOneTest("NextLine_byte") }
  @Test def test_FormFeed() { runner.runOneTest("FormFeed") }
  
  val testDir_01 = "/daffodil/section06/entities/"
  val tdml_01 = testDir_01 + "Entities.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  @Test def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  @Test def test_text_entities_6_03() { runner_01.runOneTest("text_entities_6_03") }
  @Test def test_text_entities_6_04() { runner_01.runOneTest("text_entities_6_04") }
  @Test def test_byte_entities_6_01() { runner_01.runOneTest("byte_entities_6_01") }
  @Test def test_byte_entities_6_02() { runner_01.runOneTest("byte_entities_6_02") }
  @Test def test_byte_entities_6_03() { runner_01.runOneTest("byte_entities_6_03") }
  @Test def test_byte_entities_6_04() { runner_01.runOneTest("byte_entities_6_04") }
  @Test def test_byte_entities_6_05() { runner_01.runOneTest("byte_entities_6_05") }
  //@Test def test_byte_entities_6_06() { runner_01.runOneTest("byte_entities_6_06") }
  //@Test def test_byte_entities_6_07() { runner_01.runOneTest("byte_entities_6_07") }
  
  val testDir_02 = "/daffodil/ibm-tests/"
  val tdml_02 = testDir_02 + "dpaext1.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  @Test def test_syntax_entities_6_01() { runner_02.runOneTest("syntax_entities_6_01") }
  @Test def test_syntax_entities_6_02() { runner_02.runOneTest("syntax_entities_6_02") }
  @Test def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }
  
  val entity = testDir + "entities_01.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  @Test def test_entity_fail_01() { runnerEntity.runOneTest("entity_fail_01") }
  @Test def test_entity_fail_02() { runnerEntity.runOneTest("entity_fail_02") }
  @Test def test_entity_fail_03() { runnerEntity.runOneTest("entity_fail_03") }
  @Test def test_entity_fail_04() { runnerEntity.runOneTest("entity_fail_04") }
  
  val testDir_03 = "/daffodil/section13/nillable/"
  val ln = testDir_03 + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))
  @Test def test_text_02() { runnerLN.runOneTest("text_02")}
  
  }
