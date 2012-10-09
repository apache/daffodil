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

class TestLengthKindDelimitedDebug extends JUnitSuite {
  val testDir = "/daffodil/section12/lengthKind/"
  
  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))
    
  @Test def test_AB004() { runnerAB.runOneTest("AB004") }
  @Test def test_AB005() { runnerAB.runOneTest("AB005") }
  
  val aa = testDir + "DelimitedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  @Test def test_lengthKindDelimited_03() { runner.runOneTest("lengthKindDelimited_03") }
  @Test def test_lengthKindDelimited_04() { runner.runOneTest("lengthKindDelimited_04") }
  @Test def test_NumSeq_09() { runner.runOneTest("NumSeq_09") }
  @Test def test_NumSeq_10() { runner.runOneTest("NumSeq_10") }
  }
