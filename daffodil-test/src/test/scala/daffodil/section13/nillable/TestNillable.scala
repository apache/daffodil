package daffodil.section13.nillable


import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestNillable extends JUnit3Suite {
  val testDir = "/daffodil/section13/nillable/"
  val aa = testDir + "nillable.tdml"
  lazy val runnerAA = new DFDLTestSuite(Misc.getRequiredResource(aa))

  def test_litNil1() { runnerAA.runOneTest("litNil1") }
  def test_litNil2() { runnerAA.runOneTest("litNil2") }
  def test_litNil3() { runnerAA.runOneTest("litNil3") }
  
  val ln = testDir + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))
  def test_text_01() { runnerLN.runOneTest("text_01")}
  def test_text_03() { runnerLN.runOneTest("text_03")}
  def test_text_04() { runnerLN.runOneTest("text_04")}
  def test_text_05() { runnerLN.runOneTest("text_05")}
  def test_text_06() { runnerLN.runOneTest("text_06")}
  def test_binary_01() { runnerLN.runOneTest("binary_01")}
  
  val testDir_01 = "/daffodil/section06/entities/"
  val entity = testDir_01 + "entities_01.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  def test_entity_fail_05() { runnerEntity.runOneTest("entity_fail_05") }
  def test_entity_fail_06() { runnerEntity.runOneTest("entity_fail_06") }

}
