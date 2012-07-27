package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite

import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc

class IBMTestsThatPass extends JUnit3Suite {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  def test_introduction_1_01() { runner1.runOneTest("introduction_1_01") }
  def test_introduction_1_02() { runner1.runOneTest("introduction_1_02") }
  
  def test_schema_types_5_01() { runner1.runOneTest("schema_types_5_01") }
  def test_schema_types_5_02() { runner1.runOneTest("schema_types_5_02") }
  def test_schema_types_5_03() { runner1.runOneTest("schema_types_5_03") }
  def test_schema_types_5_04() { runner1.runOneTest("schema_types_5_04") }
  def test_schema_types_5_05() { runner1.runOneTest("schema_types_5_05") }
  
  def test_syntax_entities_6_01() { runner1.runOneTest("syntax_entities_6_01") }
  def test_syntax_entities_6_02() { runner1.runOneTest("syntax_entities_6_02") }
  def test_syntax_entities_6_03() { runner1.runOneTest("syntax_entities_6_03") }
  
  def test_property_syntax_7_04() { runner1.runOneTest("property_syntax_7_04") }

  def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") }
  def test_scoping_define_format_8_01() { runner1.runOneTest("scoping_define_format_8_01") }
  
  def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  def test_encoding_11_02() { runner1.runOneTest("encoding_11_02") }
  def test_encoding_11_03() { runner1.runOneTest("encoding_11_03") }
  
  def test_length_implicit_12_01() { runner1.runOneTest("length_implicit_12_01") }

  def test_length_explicit_12_01() { runner1.runOneTest("length_explicit_12_01") }
  def test_length_explicit_12_02() { runner1.runOneTest("length_explicit_12_02") }
  def test_length_explicit_12_03() { runner1.runOneTest("length_explicit_12_03") }
  def test_length_delimited_12_02() { runner1.runOneTest("length_delimited_12_02") }
  def test_length_delimited_12_03() { runner1.runOneTest("length_delimited_12_03") }

  def test_delimiter_12_03() { runner1.runOneTest("delimiter_12_03") }
  def test_delimiter_12_04() { runner1.runOneTest("delimiter_12_04") }
  
  def test_simple_type_properties_binary_number_13_02() { runner2.runOneTest("simple_type_properties_binary_number_13_02") }

  def test_sequences_separated_14_01() { runner2.runOneTest("sequences_separated_14_01") }
  def test_sequences_separated_14_02() { runner2.runOneTest("sequences_separated_14_02") }
  def test_sequences_separated_14_07() { runner2.runOneTest("sequences_separated_14_07") }
  def test_sequences_separated_14_08() { runner2.runOneTest("sequences_separated_14_08") }
}


