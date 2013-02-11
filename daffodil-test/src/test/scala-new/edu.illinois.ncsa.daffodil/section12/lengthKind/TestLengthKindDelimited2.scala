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

class TestLengthKindDelimited2 extends JUnitSuite {
  val testDir = "/daffodil/section12/lengthKind/"

  val aa = testDir + "DelimitedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_nested_NumSeq_01() = { runner.runOneTest("nested_NumSeq_01") }

}