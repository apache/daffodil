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

class TestEscapes extends JUnitSuite {
  val testDir = "/daffodil/section31/escape_characters/"
  val tdml = testDir + "Escapes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  @Test def test_escapes01() { runner.runOneTest("escape_entry1") }
  @Test def test_escapes02() { runner.runOneTest("escape_entry2") }
  @Test def test_escapes03() { runner.runOneTest("escape_entry3") }
  @Test def test_escapes04() { runner.runOneTest("escape_entry4") }
  @Test def test_escapes05() { runner.runOneTest("escape_entry5") }
  @Test def test_escapes06() { runner.runOneTest("escape_entry6") }
  @Test def test_escapes07() { runner.runOneTest("escape_entry7") }
  @Test def test_escapes08() { runner.runOneTest("escape_entry8") }
  @Test def test_escapes09() { runner.runOneTest("escape_entry9") }
//  @Test def test_escapes10() { runner.runOneTest("escape_entry10") }
  @Test def test_escapes11() { runner.runOneTest("escape_entry11") }
  
  @Test def test_escapes2_01() { runner.runOneTest("escape_entry2-1") }
  @Test def test_escapes2_02() { runner.runOneTest("escape_entry2-2") }
  @Test def test_escapes2_03() { runner.runOneTest("escape_entry2-3") }
  @Test def test_escapes2_04() { runner.runOneTest("escape_entry2-4") }
  @Test def test_escapes2_05() { runner.runOneTest("escape_entry2-5") }
  @Test def test_escapes2_06() { runner.runOneTest("escape_entry2-6") }
  @Test def test_escapes2_07() { runner.runOneTest("escape_entry2-7") }
  @Test def test_escapes2_08() { runner.runOneTest("escape_entry2-8") }
  @Test def test_escapes2_09() { runner.runOneTest("escape_entry2-9") }
//  @Test def test_escapes2_10() { runner.runOneTest("escape_entry2-10") }
  @Test def test_escapes2_11() { runner.runOneTest("escape_entry2-11") }
  @Test def test_escapes2_12() { runner.runOneTest("escape_entry2-12") }
  @Test def test_escapes2_13() { runner.runOneTest("escape_entry2-13") }
//  @Test def test_escapes2_14() { runner.runOneTest("escape_entry2-14") }
  @Test def test_escapes2_15() { runner.runOneTest("escape_entry2-15") }
  }
