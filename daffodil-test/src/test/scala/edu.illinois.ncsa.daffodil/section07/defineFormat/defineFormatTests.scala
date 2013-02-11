package daffodil.section07.defineFormat

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

class defineFormatTests extends JUnitSuite {
  val testDir = "/daffodil/section07/defineFormat/"
  val tdml = testDir + "defineFormat.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_defineFormat_01() { runner.runOneTest("defineFormat_01") }
  @Test def test_Lesson3_defineFormat() { runner.runOneTest("Lesson3_defineFormat") }
  @Test def test_Lesson3_inherit_defineFormat() { runner.runOneTest("Lesson3_inherit_defineFormat") }
//  @Test def test_nameCollision() { runner.runOneTest("nameCollision") }
  @Test def test_formatOnlyDefine() { runner.runOneTest("formatOnlyDefine") }
  @Test def test_circularRef() { runner.runOneTest("circularRef") }
  @Test def test_noNameFormat() { runner.runOneTest("noNameFormat") }

}
