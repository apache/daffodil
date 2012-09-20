package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite

import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc

class IBMTestsThatPass extends JUnit3Suite {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  def test_introduction_1_01() { runner1.runOneTest("introduction_1_01") }
  
  def test_property_syntax_7_04() { runner1.runOneTest("property_syntax_7_04") }

  def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") }
  def test_scoping_define_format_8_01() { runner1.runOneTest("scoping_define_format_8_01") }
  
  def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  
  def test_length_implicit_12_01() { runner1.runOneTest("length_implicit_12_01") }

  def test_length_explicit_12_03() { runner1.runOneTest("length_explicit_12_03") }
  
  def test_simple_type_properties_binary_number_13_02() { runner2.runOneTest("simple_type_properties_binary_number_13_02") }

  def test_sequences_separated_14_01() { runner2.runOneTest("sequences_separated_14_01") }
  def test_sequences_separated_14_02() { runner2.runOneTest("sequences_separated_14_02") }
  def test_sequences_separated_14_07() { runner2.runOneTest("sequences_separated_14_07") }
  def test_sequences_separated_14_08() { runner2.runOneTest("sequences_separated_14_08") }
}


