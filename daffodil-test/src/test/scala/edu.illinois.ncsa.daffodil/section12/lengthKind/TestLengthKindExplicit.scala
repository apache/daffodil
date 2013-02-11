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
import edu.illinois.ncsa.daffodil.debugger.Debugger.withDebugger
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestLengthKindExplicit extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section12/lengthKind/"
  val aa = testDir + "ExplicitTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_Lesson1_lengthKind_explicit() { runner.runOneTest("Lesson1_lengthKind_explicit") }
  @Test def test_ExplicitLengthBytesNotFixed() = { runner.runOneTest("test_ExplicitLengthBytesNotFixed") }
  @Test def test_ExplicitLengthBytesFixed() = { runner.runOneTest("test_ExplicitLengthBytesFixed") }
  @Test def test_ExplicitLengthBytesBroken() = { runner.runOneTest("test_ExplicitLengthBytesBroken") }
  @Test def test_ExplicitLengthBytesChoiceRef() = { runner.runOneTest("test_ExplicitLengthBytesChoiceRef") }

}
