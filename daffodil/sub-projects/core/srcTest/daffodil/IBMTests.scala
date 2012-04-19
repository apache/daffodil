package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite

import daffodil.tdml.DFDLTestSuite

class IBMTestsThatPass extends JUnit3Suite {

  val testDir = "test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  val runner1 = new DFDLTestSuite(new File(tdml1))
  val runner2 = new DFDLTestSuite(new File(tdml2))

  def test_introduction_1_01() { runner1.runOneTest("introduction_1_01") }
  def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  def test_encoding_11_02() { runner1.runOneTest("encoding_11_02") }
  def test_encoding_11_03() { runner1.runOneTest("encoding_11_03") }
  def test_length_implicit_12_01() { runner1.runOneTest("length_implicit_12_01") }
    
}
 
class IBMTestsThatThrow extends JUnit3Suite {

  val testDir = "test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  val runner1 = new DFDLTestSuite(new File(tdml1))
  val runner2 = new DFDLTestSuite(new File(tdml2))
  def test_introduction_1_02() { runner1.runOneTest("introduction_1_02") }
  def test_schema_types_5_01() { runner1.runOneTest("schema_types_5_01") }
  def test_schema_types_5_02() { runner1.runOneTest("schema_types_5_02") }
  def test_schema_types_5_03() { runner1.runOneTest("schema_types_5_03") }
  def test_schema_types_5_04() { runner1.runOneTest("schema_types_5_04") }
  def test_schema_types_5_05() { runner1.runOneTest("schema_types_5_05") }
  def test_syntax_entities_6_01() { runner1.runOneTest("syntax_entities_6_01") }
  def test_syntax_entities_6_02() { runner1.runOneTest("syntax_entities_6_02") }
  def test_syntax_entities_6_03() { runner1.runOneTest("syntax_entities_6_03") }
  def test_syntax_entities_6_04() { runner1.runOneTest("syntax_entities_6_04") }
  def test_property_syntax_7_01() { runner1.runOneTest("property_syntax_7_01") }
  def test_property_syntax_7_02() { runner1.runOneTest("property_syntax_7_02") }
  def test_property_syntax_7_03() { runner1.runOneTest("property_syntax_7_03") }
  def test_property_syntax_7_04() { runner1.runOneTest("property_syntax_7_04") }
  def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") }
  def test_scoping_define_format_8_01() { runner1.runOneTest("scoping_define_format_8_01") }
  def test_scoping_define_format_8_02() { runner1.runOneTest("scoping_define_format_8_02") }
  def test_scoping_define_format_8_03() { runner1.runOneTest("scoping_define_format_8_03") }
  def test_scoping_define_format_8_04() { runner1.runOneTest("scoping_define_format_8_04") }
  def test_scoping_define_format_8_05() { runner1.runOneTest("scoping_define_format_8_05") }
  def test_alignment_bytes_12_01() { runner1.runOneTest("alignment_bytes_12_01") }
  def test_alignment_bytes_12_02() { runner1.runOneTest("alignment_bytes_12_02") }
  def test_alignment_bytes_12_03() { runner1.runOneTest("alignment_bytes_12_03") }
  def test_alignment_bytes_12_04() { runner1.runOneTest("alignment_bytes_12_04") }
  def test_alignment_bytes_12_05() { runner1.runOneTest("alignment_bytes_12_05") }
  def test_alignment_bytes_12_06() { runner1.runOneTest("alignment_bytes_12_06") }
  def test_delimiter_12_01() { runner1.runOneTest("delimiter_12_01") }
  def test_delimiter_12_02() { runner1.runOneTest("delimiter_12_02") }
  def test_delimiter_12_03() { runner1.runOneTest("delimiter_12_03") }
  def test_delimiter_12_04() { runner1.runOneTest("delimiter_12_04") }
  def test_length_explicit_12_01() { runner1.runOneTest("length_explicit_12_01") }
  def test_length_explicit_12_02() { runner1.runOneTest("length_explicit_12_02") }
  def test_length_delimited_12_01() { runner1.runOneTest("length_delimited_12_01") }
  def test_length_delimited_12_02() { runner1.runOneTest("length_delimited_12_02") }
  def test_length_delimited_12_03() { runner1.runOneTest("length_delimited_12_03") }
  def test_length_delimited_12_04() { runner1.runOneTest("length_delimited_12_04") }
  def test_length_delimited_12_05() { runner1.runOneTest("length_delimited_12_05") }
  def test_length_delimited_12_06() { runner1.runOneTest("length_delimited_12_06") }
  def test_length_implicit_12_02() { runner1.runOneTest("length_implicit_12_02") }
  def test_length_explicit_12_03() { runner1.runOneTest("length_explicit_12_03") }

  def test_simple_type_properties_pad_trim_13_01() { runner2.runOneTest("simple_type_properties_pad_trim_13_01") }
  def test_simple_type_properties_pad_trim_13_02() { runner2.runOneTest("simple_type_properties_pad_trim_13_02") }
  def test_simple_type_properties_pad_trim_13_03() { runner2.runOneTest("simple_type_properties_pad_trim_13_03") }
  def test_simple_type_properties_pad_trim_13_04() { runner2.runOneTest("simple_type_properties_pad_trim_13_04") }
  def test_simple_type_properties_text_number_13_01() { runner2.runOneTest("simple_type_properties_text_number_13_01") }
  def test_simple_type_properties_text_number_13_02() { runner2.runOneTest("simple_type_properties_text_number_13_02") }
  def test_simple_type_properties_text_number_13_03() { runner2.runOneTest("simple_type_properties_text_number_13_03") }
  def test_simple_type_properties_binary_number_13_01() { runner2.runOneTest("simple_type_properties_binary_number_13_01") }
  def test_simple_type_properties_binary_number_13_02() { runner2.runOneTest("simple_type_properties_binary_number_13_02") }
  def test_simple_type_properties_text_boolean_13_01() { runner2.runOneTest("simple_type_properties_text_boolean_13_01") }
  def test_simple_type_properties_text_boolean_13_02() { runner2.runOneTest("simple_type_properties_text_boolean_13_02") }
  def test_simple_type_properties_text_boolean_13_03() { runner2.runOneTest("simple_type_properties_text_boolean_13_03") }
  def test_simple_type_properties_bin_boolean_13_01() { runner2.runOneTest("simple_type_properties_bin_boolean_13_01") }
  def test_simple_type_properties_text_calendar_13_01() { runner2.runOneTest("simple_type_properties_text_calendar_13_01") }
  def test_simple_type_properties_text_calendar_13_02() { runner2.runOneTest("simple_type_properties_text_calendar_13_02") }
  def test_simple_type_properties_text_calendar_13_03() { runner2.runOneTest("simple_type_properties_text_calendar_13_03") }
  def test_simple_type_properties_text_calendar_13_04() { runner2.runOneTest("simple_type_properties_text_calendar_13_04") }
  def test_simple_type_properties_bin_calendar_13_01() { runner2.runOneTest("simple_type_properties_bin_calendar_13_01") }
  def test_simple_type_properties_bin_calendar_13_02() { runner2.runOneTest("simple_type_properties_bin_calendar_13_02") }
  def test_sequences_separated_14_01() { runner2.runOneTest("sequences_separated_14_01") }
  def test_sequences_separated_14_02() { runner2.runOneTest("sequences_separated_14_02") }
  def test_sequences_separated_14_03() { runner2.runOneTest("sequences_separated_14_03") }
  def test_sequences_separated_14_04() { runner2.runOneTest("sequences_separated_14_04") }
  def test_sequences_separated_14_05() { runner2.runOneTest("sequences_separated_14_05") }
  def test_sequences_separated_14_06() { runner2.runOneTest("sequences_separated_14_06") }
  def test_sequences_separated_14_07() { runner2.runOneTest("sequences_separated_14_07") }
  def test_sequences_separated_14_08() { runner2.runOneTest("sequences_separated_14_08") }
  def test_choices_basic_15_01() { runner2.runOneTest("choices_basic_15_01") }
  def test_choices_basic_15_02() { runner2.runOneTest("choices_basic_15_02") }
  def test_choices_basic_15_03() { runner2.runOneTest("choices_basic_15_03") }
  def test_arrays_16_01() { runner2.runOneTest("arrays_16_01") }

}