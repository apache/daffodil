package daffodil

import java.io.File
import org.scalatest.junit.JUnitSuite
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import org.junit.Test

/**
 * Delete this class once these regressions have been addressed.
 */
//class IBMTestRegressions extends JUnitSuite {
//  
//  val testDir = "/test-suite/ibm-contributed/"
//  val tdml1 = testDir + "dpaext1.tdml"
//  val tdml2 = testDir + "dpaext2.tdml"
//  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
//  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
//
//  // Nothing here! Great!
//}

class IBMTestsThatPass2 extends JUnitSuite {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_property_syntax_7_01() { runner1.runOneTest("property_syntax_7_01") }
  @Test def test_property_syntax_7_02() { runner1.runOneTest("property_syntax_7_02") }
  @Test def test_property_syntax_7_03() { runner1.runOneTest("property_syntax_7_03") }

  @Test def test_delimiter_12_01() { runner1.runOneTest("delimiter_12_01") }

  @Test def test_length_delimited_12_06() { runner1.runOneTest("length_delimited_12_06") }

  @Test def test_sequences_separated_14_03() { runner2.runOneTest("sequences_separated_14_03") }

  @Test def test_sequences_separated_14_05() { runner2.runOneTest("sequences_separated_14_05") }
  @Test def test_sequences_separated_14_06() { runner2.runOneTest("sequences_separated_14_06") }

  @Test def test_choices_basic_15_01() { runner2.runOneTest("choices_basic_15_01") }
  @Test def test_choices_basic_15_02() { runner2.runOneTest("choices_basic_15_02") }
  @Test def test_choices_basic_15_03() { runner2.runOneTest("choices_basic_15_03") }
  @Test def test_arrays_16_01() { runner2.runOneTest("arrays_16_01") }

}
