package daffodil.section06.entities

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestEntities extends JUnit3Suite {
  val testDir = "/daffodil/section06/entities/"
  val tdml = testDir + "charClassEntities.tdml"
  val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  def test_LineFeed() { runner.runOneTest("LineFeed") }
  def test_CarriageReturn() { runner.runOneTest("CarriageReturn") }
  def test_LineSeparator() { runner.runOneTest("LineSeparator") }
  def test_NextLine() { runner.runOneTest("NextLine") }
  def test_LineFeed_byte() { runner.runOneTest("LineFeed_byte") }
  def test_CarriageReturn_byte() { runner.runOneTest("CarriageReturn_byte") }
  def test_CRLF_byte() { runner.runOneTest("CRLF_byte") }
  def test_LineSeparator_byte() { runner.runOneTest("LineSeparator_byte") }
  def test_NextLine_byte() { runner.runOneTest("NextLine_byte") }
  def test_FormFeed() { runner.runOneTest("FormFeed") }
  
  val testDir_01 = "/daffodil/section06/entities/"
  val tdml_01 = testDir_01 + "Entities.tdml"
  val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  //def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  
  val testDir_02 = "/daffodil/ibm-tests/"
  val tdml_02 = testDir_02 + "dpaext1.tdml"
  val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  def test_syntax_entities_6_01() { runner_02.runOneTest("syntax_entities_6_01") }
  def test_syntax_entities_6_02() { runner_02.runOneTest("syntax_entities_6_02") }
  def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }
  
  }
