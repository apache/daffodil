package daffodil.section13.nillable

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

class TestNillable extends JUnitSuite {
  val testDir = "/daffodil/section13/nillable/"
  val aa = testDir + "nillable.tdml"
  lazy val runnerAA = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_litNil1() { runnerAA.runOneTest("litNil1") }
  @Test def test_litNil2() { runnerAA.runOneTest("litNil2") }
  @Test def test_litNil3() { runnerAA.runOneTest("litNil3") }
  @Test def test_litNil4() { runnerAA.runOneTest("litNil4") }
  @Test def test_litNil4b() { runnerAA.runOneTest("litNil4b") }
  @Test def test_litNil5() { runnerAA.runOneTest("litNil5") }
  @Test def test_litNil6() { runnerAA.runOneTest("litNil6") }
  @Test def test_missing_scalar() { runnerAA.runOneTest("missing_scalar") }
  @Test def test_nillable1() { runnerAA.runOneTest("nillable1") }

  val ln = testDir + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))
  //@Test def test_text_01() { runnerLN.runOneTest("text_01")}  This test is identical to litNil1.
  //@Test def test_text_02() { runnerLN.runOneTest("text_02")}  This test is identical to litNil2
  @Test def test_text_03() { runnerLN.runOneTest("text_03") }
  @Test def test_text_04() { runnerLN.runOneTest("text_04") }
  @Test def test_text_05() { runnerLN.runOneTest("text_05") }
  @Test def test_text_06() = { runnerLN.runOneTest("text_06") }
  @Test def test_binary_01() = { runnerLN.runOneTest("binary_01") }

  val testDir_01 = "/daffodil/section06/entities/"
  val entity = testDir_01 + "entities_01.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  @Test def test_entity_fail_05() { runnerEntity.runOneTest("entity_fail_05") }
  @Test def test_entity_fail_06() { runnerEntity.runOneTest("entity_fail_06") }
  @Test def test_entity_success_05() { runnerEntity.runOneTest("entity_success_05") }
  @Test def test_entity_success_06() { runnerEntity.runOneTest("entity_success_06") }

}
