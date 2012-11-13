package daffodil.section31.escape_characters

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

class TestEscapes2 extends JUnitSuite {
  val testDir = "/daffodil/section31/escape_characters/"
  val tdml = testDir + "Escapes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_escapes10() { runner.runOneTest("escape_entry10") }
  @Test def test_escapes2_10() { runner.runOneTest("escape_entry2-10") }
  @Test def test_escapes2_14() { runner.runOneTest("escape_entry2-14") }

}
