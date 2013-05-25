package edu.illinois.ncsa.daffodil
import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test

/**
 * Delete this class once these regressions have been addressed.
 */
//class IBMTestRegressions {
//  
//  val testDir = "/test-suite/ibm-contributed/"
//  val tdml1 = testDir + "dpaext1.tdml"
//  val tdml2 = testDir + "dpaext2.tdml"
//  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
//  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
//
//  // Nothing here! Great!
//}

class IBMTestsThatThrow {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_scoping_define_format_8_02() { runner1.runOneTest("scoping_define_format_8_02") } //packed
  @Test def test_scoping_define_format_8_03() { runner1.runOneTest("scoping_define_format_8_03") } //packed
  @Test def test_scoping_define_format_8_04() { runner1.runOneTest("scoping_define_format_8_04") } //attributeFormDefault='qualified'

  @Test def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") } // attributeFormDefault='qualified'
  @Test def test_scoping_define_format_8_01() { runner1.runOneTest("scoping_define_format_8_01") } // attributeFormDefault='qualified'

  @Test def test_alignment_bytes_12_04() { runner1.runOneTest("alignment_bytes_12_04") } //boolean
  @Test def test_alignment_bytes_12_05() { runner1.runOneTest("alignment_bytes_12_05") } //binary dateTime
  @Test def test_delimiter_12_02() { runner1.runOneTest("delimiter_12_02") } //ignoreCase='yes'

  @Test def test_length_implicit_12_02() { runner1.runOneTest("length_implicit_12_02") } // implicit length string - bug in IBM test (doesn't have minLength - both are required)

  @Test def test_length_delimited_12_05() { runner1.runOneTest("length_delimited_12_05") } // decimal

  @Test def test_simple_type_properties_text_number_13_01() { runner2.runOneTest("simple_type_properties_text_number_13_01") } // decimal
  @Test def test_simple_type_properties_text_number_13_02() { runner2.runOneTest("simple_type_properties_text_number_13_02") } // textStandardInfinityRep, textStandardZeroRep, textNumberPattern
  @Test def test_simple_type_properties_text_number_13_03() { runner2.runOneTest("simple_type_properties_text_number_13_03") } // textStandardBase (base 16)
  @Test def test_simple_type_properties_binary_number_13_01() { runner2.runOneTest("simple_type_properties_binary_number_13_01") } // decimal

  @Test def test_simple_type_properties_text_boolean_13_01() { runner2.runOneTest("simple_type_properties_text_boolean_13_01") } // boolean type
  @Test def test_simple_type_properties_text_boolean_13_02() { runner2.runOneTest("simple_type_properties_text_boolean_13_02") } // boolean type
  @Test def test_simple_type_properties_text_boolean_13_03() { runner2.runOneTest("simple_type_properties_text_boolean_13_03") } // boolean type
  @Test def test_simple_type_properties_bin_boolean_13_01() { runner2.runOneTest("simple_type_properties_bin_boolean_13_01") } // boolean type
  @Test def test_simple_type_properties_bin_calendar_13_01() { runner2.runOneTest("simple_type_properties_bin_calendar_13_01") } // dateTime
  @Test def test_simple_type_properties_bin_calendar_13_02() { runner2.runOneTest("simple_type_properties_bin_calendar_13_02") } // dateTime

  @Test def test_sequences_separated_14_04() { runner2.runOneTest("sequences_separated_14_04") } // left over data

}
