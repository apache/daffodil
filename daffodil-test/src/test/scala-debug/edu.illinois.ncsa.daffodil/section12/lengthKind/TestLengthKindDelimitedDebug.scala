package edu.illinois.ncsa.daffodil.section12.lengthKind

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthKindDelimitedDebug extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section12/lengthKind/"

  val aa = testDir + "DelimitedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  @Test def test_lengthKindDelimited_03() { runner.runOneTest("lengthKindDelimited_03") }
  @Test def test_lengthKindDelimited_04() { runner.runOneTest("lengthKindDelimited_04") }
  @Test def test_NumSeq_10() { runner.runOneTest("NumSeq_10") }
  //@Test def test_NumSeq_02() { runner.runOneTest("NumSeq_02") }
  //@Test def test_nested_NumSeq_01() =  { runner.runOneTest("nested_NumSeq_01") }

}
