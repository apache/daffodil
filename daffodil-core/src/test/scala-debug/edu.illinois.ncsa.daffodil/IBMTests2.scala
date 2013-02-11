package edu.illinois.ncsa.daffodil

import java.io.File
import org.scalatest.junit.JUnitSuite
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
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

class IBMTestsThatThrow extends JUnitSuite {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_syntax_entities_6_04() { runner1.runOneTest("syntax_entities_6_04") } // needs alignment

  @Test def test_scoping_define_format_8_02() { runner1.runOneTest("scoping_define_format_8_02") } //packed
  @Test def test_scoping_define_format_8_03() { runner1.runOneTest("scoping_define_format_8_03") } //packed
  @Test def test_scoping_define_format_8_04() { runner1.runOneTest("scoping_define_format_8_04") } //alignment
  @Test def test_scoping_define_format_8_05() { runner1.runOneTest("scoping_define_format_8_05") } //multi-file schemas
  @Test def test_alignment_bytes_12_01() { runner1.runOneTest("alignment_bytes_12_01") } //alignment
  @Test def test_alignment_bytes_12_02() { runner1.runOneTest("alignment_bytes_12_02") } //alignment
  @Test def test_alignment_bytes_12_03() { runner1.runOneTest("alignment_bytes_12_03") } //alignment
  @Test def test_alignment_bytes_12_04() { runner1.runOneTest("alignment_bytes_12_04") } //alignment
  @Test def test_alignment_bytes_12_05() { runner1.runOneTest("alignment_bytes_12_05") } //alignment
  @Test def test_alignment_bytes_12_06() { runner1.runOneTest("alignment_bytes_12_06") } //alignment
  @Test def test_delimiter_12_02() { runner1.runOneTest("delimiter_12_02") } //ignoreCase='yes'

  @Test def test_length_delimited_12_01() { runner1.runOneTest("length_delimited_12_01") } // date

  @Test def test_length_delimited_12_04() { runner1.runOneTest("length_delimited_12_04") } // date
  @Test def test_length_delimited_12_05() { runner1.runOneTest("length_delimited_12_05") } // decimal
  @Test def test_length_implicit_12_02() { runner1.runOneTest("length_implicit_12_02") } // textual data with implicit length (for strings, comes from maxLength facet)

  @Test def test_simple_type_properties_pad_trim_13_01() { runner2.runOneTest("simple_type_properties_pad_trim_13_01") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_02() { runner2.runOneTest("simple_type_properties_pad_trim_13_02") } // xs:integer type
  @Test def test_simple_type_properties_pad_trim_13_03() { runner2.runOneTest("simple_type_properties_pad_trim_13_03") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_04() { runner2.runOneTest("simple_type_properties_pad_trim_13_04") } // pad/trim
  @Test def test_simple_type_properties_text_number_13_01() { runner2.runOneTest("simple_type_properties_text_number_13_01") } // decimal
  @Test def test_simple_type_properties_text_number_13_02() { runner2.runOneTest("simple_type_properties_text_number_13_02") } // textStandardInfinityRep, textStandardZeroRep, textNumberPattern
  @Test def test_simple_type_properties_text_number_13_03() { runner2.runOneTest("simple_type_properties_text_number_13_03") } // textStandardBase (base 16)
  @Test def test_simple_type_properties_binary_number_13_01() { runner2.runOneTest("simple_type_properties_binary_number_13_01") } // decimal

  @Test def test_simple_type_properties_text_boolean_13_01() { runner2.runOneTest("simple_type_properties_text_boolean_13_01") } // boolean type
  @Test def test_simple_type_properties_text_boolean_13_02() { runner2.runOneTest("simple_type_properties_text_boolean_13_02") } // boolean type
  @Test def test_simple_type_properties_text_boolean_13_03() { runner2.runOneTest("simple_type_properties_text_boolean_13_03") } // boolean type
  @Test def test_simple_type_properties_bin_boolean_13_01() { runner2.runOneTest("simple_type_properties_bin_boolean_13_01") } // boolean type
  @Test def test_simple_type_properties_text_calendar_13_01() { runner2.runOneTest("simple_type_properties_text_calendar_13_01") } // dateTime
  @Test def test_simple_type_properties_text_calendar_13_02() { runner2.runOneTest("simple_type_properties_text_calendar_13_02") } // dateTime
  @Test def test_simple_type_properties_text_calendar_13_03() { runner2.runOneTest("simple_type_properties_text_calendar_13_03") } // dateTime
  @Test def test_simple_type_properties_text_calendar_13_04() { runner2.runOneTest("simple_type_properties_text_calendar_13_04") } // dateTime
  @Test def test_simple_type_properties_bin_calendar_13_01() { runner2.runOneTest("simple_type_properties_bin_calendar_13_01") } // dateTime
  @Test def test_simple_type_properties_bin_calendar_13_02() { runner2.runOneTest("simple_type_properties_bin_calendar_13_02") } // dateTime

  @Test def test_sequences_separated_14_04() { runner2.runOneTest("sequences_separated_14_04") } // left over data

}
