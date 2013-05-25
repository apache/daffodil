package edu.illinois.ncsa.daffodil
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test
import org.junit.Test

class IBMTestsThatPass2 {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_alignment_bytes_12_01() { runner1.runOneTest("alignment_bytes_12_01") } //alignment
  @Test def test_alignment_bytes_12_02() { runner1.runOneTest("alignment_bytes_12_02") } //alignment
  @Test def test_alignment_bytes_12_03() { runner1.runOneTest("alignment_bytes_12_03") } //alignment

  @Test def test_alignment_bytes_12_06() { runner1.runOneTest("alignment_bytes_12_06") } //alignment

  @Test def test_syntax_entities_6_04() { runner1.runOneTest("syntax_entities_6_04") } // alignment

  @Test def test_scoping_define_format_8_05() { runner1.runOneTest("scoping_define_format_8_05") } //multi-file schemas DFDL-552 and DFDL-503    
}

