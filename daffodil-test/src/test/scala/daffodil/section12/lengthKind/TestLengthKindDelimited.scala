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

class TestLengthKindDelimited extends JUnitSuite {
  val testDir = "/daffodil/section12/lengthKind/"
  val aa = testDir + "DelimitedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_NumSeq_00a() = {
    runner.runOneTest("NumSeq_00a")
  }
  @Test def test_NumSeq_00nl() = {
    runner.runOneTest("NumSeq_00nl")
  }
  @Test def test_NumSeq_01() = {
    runner.runOneTest("NumSeq_01")
  }
  @Test def test_NumSeq_03() { runner.runOneTest("NumSeq_03") }
  @Test def test_NumSeq_04() { runner.runOneTest("NumSeq_04") }
  @Test def test_NumSeq_05() { runner.runOneTest("NumSeq_05") }
  @Test def test_NumSeq_06() { runner.runOneTest("NumSeq_06") }
  @Test def test_NumSeq_07() { runner.runOneTest("NumSeq_07") }
  @Test def test_NumSeq_08() { runner.runOneTest("NumSeq_08") }
  @Test def test_lengthKindDelimited_01() { runner.runOneTest("lengthKindDelimited_01") }
  @Test def test_lengthKindDelimited_02() { runner.runOneTest("lengthKindDelimited_02") }
  //@Test def test_lengthKindDelimited_03() { runner.runOneTest("lengthKindDelimited_03") }
  //@Test def test_lengthKindDelimited_04() { runner.runOneTest("lengthKindDelimited_04") }
  @Test def test_NumSeq_11() { runner.runOneTest("NumSeq_11") }
  @Test def test_NumSeq_12() { runner.runOneTest("NumSeq_12") }
  @Test def test_NumSeq_13() { runner.runOneTest("NumSeq_13") }
  @Test def test_NumSeq_14() { runner.runOneTest("NumSeq_14") }
  // Tests that initiator is found when on ElementRef
  @Test def test_refInitiator() { runner.runOneTest("refInitiator") }
  // Tests that initiator is found when on GlobalElmentDecl
  @Test def test_refInitiator2() { runner.runOneTest("refInitiator2") }
  @Test def test_binary_delimited_fail() { runner.runOneTest("binary_delimited_fail") }

  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))

  @Test def test_AB000() { runnerAB.runOneTest("AB000") }
  @Test def test_AB001() { runnerAB.runOneTest("AB001") }
  @Test def test_AB002() { runnerAB.runOneTest("AB002") }
  @Test def test_AB003() { runnerAB.runOneTest("AB003") }
  @Test def test_AB004() { runnerAB.runOneTest("AB004") }
  @Test def test_AB005() { runnerAB.runOneTest("AB005") }

  val an = testDir + "AN.tdml"
  lazy val runnerAN = new DFDLTestSuite(Misc.getRequiredResource(an))

  @Test def test_AN000() { runnerAN.runOneTest("AN000") }
  @Test def test_AN001() { runnerAN.runOneTest("AN001") }

  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_introduction_1_02() { runner_01.runOneTest("introduction_1_02") }
  @Test def test_length_delimited_12_03() { runner_01.runOneTest("length_delimited_12_03") }
  @Test def test_length_delimited_12_02() { runner_01.runOneTest("length_delimited_12_02") }
  @Test def test_multiple_delimiters() { runner_01.runOneTest("multiple_delimiters") }

}
