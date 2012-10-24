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

class TestLengthKindImplicit extends JUnitSuite {
  val testDir = "/daffodil/section12/lengthKind/"
  val tdml = testDir + "implicit.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  @Test def test_nested_seq() { runner_01.runOneTest("nested_seq") }
  @Test def test_nested_seq_01() { runner_01.runOneTest("nested_seq_01") }
  
  }
