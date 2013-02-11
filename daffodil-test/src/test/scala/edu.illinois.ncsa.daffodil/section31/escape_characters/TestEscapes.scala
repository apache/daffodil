package edu.illinois.ncsa.daffodil.section31.escape_characters

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

class TestEscapes extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section31/escape_characters/"
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
  @Test def test_escapes10() { runner.runOneTest("escape_entry10") }
  @Test def test_escapes11() { runner.runOneTest("escape_entry11") }
  @Test def test_escapes12() { runner.runOneTest("escape_entry12") }

  @Test def test_escapes2_01() { runner.runOneTest("escape_entry2-1") }
  @Test def test_escapes2_02() { runner.runOneTest("escape_entry2-2") }
  @Test def test_escapes2_03() { runner.runOneTest("escape_entry2-3") }
  @Test def test_escapes2_04() { runner.runOneTest("escape_entry2-4") }
  @Test def test_escapes2_05() { runner.runOneTest("escape_entry2-5") }
  @Test def test_escapes2_06() { runner.runOneTest("escape_entry2-6") }
  @Test def test_escapes2_07() { runner.runOneTest("escape_entry2-7") }
  @Test def test_escapes2_08() { runner.runOneTest("escape_entry2-8") }
  @Test def test_escapes2_09() { runner.runOneTest("escape_entry2-9") }
  @Test def test_escapes2_10() { runner.runOneTest("escape_entry2-10") }
  @Test def test_escapes2_11() { runner.runOneTest("escape_entry2-11") }
  @Test def test_escapes2_12() { runner.runOneTest("escape_entry2-12") }
  @Test def test_escapes2_13() { runner.runOneTest("escape_entry2-13") }
  @Test def test_escapes2_14() { runner.runOneTest("escape_entry2-14") }
  @Test def test_escapes2_15() { runner.runOneTest("escape_entry2-15") }
  @Test def test_escapes2_16() { runner.runOneTest("escape_entry2-16") }
  @Test def test_escapes2_17() { runner.runOneTest("escape_entry2-17") }
  @Test def test_escapes2_18() { runner.runOneTest("escape_entry2-18") }
  
  @Test def test_escapes3_01() { runner.runOneTest("escape_entry3-1") }
  @Test def test_escapes3_02() { runner.runOneTest("escape_entry3-2") }
  @Test def test_escapes3_03() { runner.runOneTest("escape_entry3-3") }
  @Test def test_escapes3_04() { runner.runOneTest("escape_entry3-4") }
  @Test def test_escapes3_05() { runner.runOneTest("escape_entry3-5") }
  @Test def test_escapes3_06() { runner.runOneTest("escape_entry3-6") }
  @Test def test_escapes3_07() { runner.runOneTest("escape_entry3-7") }
  @Test def test_escapes3_08() { runner.runOneTest("escape_entry3-8") }
  @Test def test_escapes3_09() { runner.runOneTest("escape_entry3-9") }
  @Test def test_escapes3_10() { runner.runOneTest("escape_entry3-10") }
  @Test def test_escapes3_11() { runner.runOneTest("escape_entry3-11") }
  @Test def test_escapes3_12() { runner.runOneTest("escape_entry3-12") }
  @Test def test_escapes3_13() { runner.runOneTest("escape_entry3-13") }
  @Test def test_escapes3_14() { runner.runOneTest("escape_entry3-14") }
  @Test def test_escapes3_15() { runner.runOneTest("escape_entry3-15") }
  @Test def test_escapes3_16() { runner.runOneTest("escape_entry3-16") }
  @Test def test_escapes3_17() { runner.runOneTest("escape_entry3-17") }
  @Test def test_escapes3_18() { runner.runOneTest("escape_entry3-18") }
  @Test def test_escapes3_19() { runner.runOneTest("escape_entry3-19") }
  @Test def test_escapes3_20() { runner.runOneTest("escape_entry3-20") }
  @Test def test_escapes3_21() { runner.runOneTest("escape_entry3-21") }
  @Test def test_escapes3_22() { runner.runOneTest("escape_entry3-22") }
  @Test def test_escapes3_23() { runner.runOneTest("escape_entry3-23") }
  @Test def test_escapes3_24() { runner.runOneTest("escape_entry3-24") }
  @Test def test_escapes3_25() { runner.runOneTest("escape_entry3-25") }
  @Test def test_escapes3_26() { runner.runOneTest("escape_entry3-26") }
  @Test def test_escapes3_27() { runner.runOneTest("escape_entry3-27") }
  @Test def test_escapes3_28() { runner.runOneTest("escape_entry3-28") }
  @Test def test_escapes3_29() { runner.runOneTest("escape_entry3-29") }
  @Test def test_escapes3_30() { runner.runOneTest("escape_entry3-30") }
  @Test def test_escapes3_31() { runner.runOneTest("escape_entry3-31") }
  
  @Test def test_escape_entry4_1() { runner.runOneTest("escape_entry4-1") }
  @Test def test_escape_entry4_2() { runner.runOneTest("escape_entry4-2") }
  @Test def test_escape_entry4_3() { runner.runOneTest("escape_entry4-3") }
  @Test def test_escape_entry4_4() { runner.runOneTest("escape_entry4-4") }
  @Test def test_escape_entry4_5() { runner.runOneTest("escape_entry4-5") }
  @Test def test_escape_entry4_6() { runner.runOneTest("escape_entry4-6") }
  @Test def test_escape_entry4_7() { runner.runOneTest("escape_entry4-7") }
  @Test def test_escape_entry4_8() { runner.runOneTest("escape_entry4-8") }
  @Test def test_escape_entry4_9() { runner.runOneTest("escape_entry4-9") }
  @Test def test_escape_entry4_10() { runner.runOneTest("escape_entry4-10") }
  @Test def test_escape_entry4_11() { runner.runOneTest("escape_entry4-11") }
  @Test def test_escape_entry4_12() { runner.runOneTest("escape_entry4-12") }
  @Test def test_escape_entry4_13() { runner.runOneTest("escape_entry4-13") }
  @Test def test_escape_entry4_14() { runner.runOneTest("escape_entry4-14") }
  @Test def test_escape_entry4_15() { runner.runOneTest("escape_entry4-15") }
  @Test def test_escape_entry4_16() { runner.runOneTest("escape_entry4-16") }
  @Test def test_escape_entry4_17() { runner.runOneTest("escape_entry4-17") }
  @Test def test_escape_entry4_18() { runner.runOneTest("escape_entry4-18") }
  @Test def test_escape_entry4_19() { runner.runOneTest("escape_entry4-19") }
  @Test def test_escape_entry4_20() { runner.runOneTest("escape_entry4-20") }
  @Test def test_escape_entry4_21() { runner.runOneTest("escape_entry4-21") }
  @Test def test_escape_entry4_22() { runner.runOneTest("escape_entry4-22") }
  @Test def test_escape_entry4_23() { runner.runOneTest("escape_entry4-23") }
  @Test def test_escape_entry4_24() { runner.runOneTest("escape_entry4-24") }
  @Test def test_escape_entry4_25() { runner.runOneTest("escape_entry4-25") }
  @Test def test_escape_entry4_26() { runner.runOneTest("escape_entry4-26") }
  @Test def test_escape_entry4_27() { runner.runOneTest("escape_entry4-27") }

}
