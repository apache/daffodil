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

}
