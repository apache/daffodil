package daffodil.section6.entities

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestEntities extends JUnit3Suite {
  val testDir = "srcTest/daffodil/section6/entities/"
  val tdml = testDir + "charClassEntities.tdml"
  val runner = new DFDLTestSuite(new File(tdml))
  
  def test_LineFeed() { runner.runOneTest("LineFeed") }
  def test_CarriageReturn() { runner.runOneTest("CarriageReturn") }
  def test_LineSeparator() { runner.runOneTest("LineSeparator") }
  def test_NextLine() { runner.runOneTest("NextLine") }
  def test_FormFeed() { runner.runOneTest("FormFeed") }
  
  val testDir_01 = "srcTest/daffodil/section6/entities/"
  val tdml_01 = testDir_01 + "Entities.tdml"
  val runner_01 = new DFDLTestSuite(new File(tdml_01))
  
  //def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  
  val testDir_02 = "srcTest/daffodil/ibm-tests/"
  val tdml_02 = testDir_02 + "dpaext1.tdml"
  val runner_02 = new DFDLTestSuite(new File(tdml_02))
  def test_syntax_entities_6_01() { runner_02.runOneTest("syntax_entities_6_01") }
  def test_syntax_entities_6_02() { runner_02.runOneTest("syntax_entities_6_02") }
  def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }
  
  }
