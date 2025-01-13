/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object IBMTestsDPAEXT1 extends TdmlSuite {
  val tdmlResource = "/test-suite/ibm-contributed/dpaext1.tdml"
}

class IBMTestsDPAEXT1 extends TdmlTests {
  val tdmlSuite = IBMTestsDPAEXT1

  @Test def alignment_bytes_12_04 = test

  @Test def schema_types_5_01 = test
  @Test def schema_types_5_02 = test
  @Test def schema_types_5_03 = test
  @Test def schema_types_5_04 = test
  @Test def schema_types_5_05 = test

  @Test def multiple_delimiters2 = test
  @Test def multiple_delimiters2_err = test

  // Fails on IBM DFDL
  @Ignore @Test def multiple_delimiters2_ibm = test

  @Test def syntax_entities_6_01 = test
  @Test def syntax_entities_6_02 = test
  //
  // Needs DFDL-1559 - CR;LF output
  @Ignore @Test def syntax_entities_6_03 = test

  @Test def property_syntax_7_01 = test
  @Test def property_syntax_7_02 = test
  @Test def property_syntax_7_03 = test

  @Test def introduction_1_02 = test
  @Test def length_delimited_12_03 = test
  @Test def length_delimited_12_02 = test
  @Test def multiple_delimiters = test

  @Test def encoding_11_01 = test
  @Test def encoding_11_02 = test
  @Test def encoding_11_03 = test

  @Test def delimiter_12_01 = test
  @Test def delimiter_12_02 = test
  @Test def delimiter_12_03 = test
  @Test def delimiter_12_04 = test

  // uses lengthUnits bytes with lengthKind explicit and utf-8
  @Test def length_explicit_12_01 = test
  @Test def length_explicit_12_02 = test
  @Test def length_delimited_12_06 = test

  @Test def alignment_bytes_12_05 = test

  @Test def length_implicit_12_02 = test
}

object IBMTestsDPAEXT1NoValidate extends TdmlSuite {
  val tdmlResource = "/test-suite/ibm-contributed/dpaext1.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = true, validateDFDLSchemas = false)
}

class IBMTestsDPAEXT1NoValidate extends TdmlTests {
  val tdmlSuite = IBMTestsDPAEXT1NoValidate

  @Test def syntax_entities_6_04 = test

  @Test def alignment_bytes_12_01 = test
  @Test def alignment_bytes_12_02 = test
  @Test def alignment_bytes_12_03 = test
  @Test def alignment_bytes_12_06 = test

  @Test def length_delimited_12_01 = test
  @Test def length_delimited_12_04 = test

  @Test def introduction_1_01 = test

  @Test def property_syntax_7_04 = test

  @Test def scoping_default_format_8_01 = test
  @Test def scoping_define_format_8_01 = test
  @Test def scoping_define_format_8_02 = test
  @Test def scoping_define_format_8_03 = test
  @Test def scoping_define_format_8_04 = test
  @Test def scoping_define_format_8_05 = test

  @Test def encoding_11_01 = test

  @Test def length_implicit_12_01 = test

  @Test def length_explicit_12_03 = test

  @Test def delimiter_12_02 = test

  // DAFFODIL-1541 Need support for handling delimited data with encoding other than ISO-8859-1
  @Ignore @Test def length_delimited_12_05 = test

  // DAFFODIL-551 Needs dfdl:utf16Width='variable' implementation
  @Ignore @Test def syntax_entities_6_03 = test
}

object IBMTestsDPAEXT2 extends TdmlSuite {
  val tdmlResource = "/test-suite/ibm-contributed/dpaext2.tdml"
}

class IBMTestsDPAEXT2 extends TdmlTests {
  val tdmlSuite = IBMTestsDPAEXT2

  @Test def simple_type_properties_text_boolean_13_01 = test
  @Test def simple_type_properties_text_boolean_13_02 = test

  @Test def sequences_separated_14_03 = test
  @Test def sequences_separated_14_05 = test
  @Test def sequences_separated_14_06 = test

  @Test def choices_basic_15_01 = test
  @Test def choices_basic_15_02 = test
  @Test def choices_basic_15_03 = test

  @Test def arrays_16_01 = test

  @Test def simple_type_properties_text_boolean_13_03 = test
  @Test def simple_type_properties_bin_boolean_13_01 = test

  @Test def sequences_separated_14_04 = test

  @Test def simple_type_properties_text_calendar_13_01 = test
  @Test def simple_type_properties_text_calendar_13_02 = test
  @Test def simple_type_properties_text_calendar_13_03 = test
  @Test def simple_type_properties_text_calendar_13_04 = test

  @Test def simple_type_properties_bin_calendar_13_01 = test
  @Test def simple_type_properties_bin_calendar_13_02 = test

  @Test def simple_type_properties_pad_trim_13_01 = test
  @Test def simple_type_properties_pad_trim_13_02 = test
  @Test def simple_type_properties_pad_trim_13_03 = test
  @Test def simple_type_properties_pad_trim_13_04 = test

  @Test def simple_type_properties_text_number_13_02 = test

  @Test def simple_type_properties_binary_number_13_01 = test
  @Test def simple_type_properties_binary_number_13_02 = test

  @Test def sequences_separated_14_01 = test
  @Test def sequences_separated_14_02 = test
  @Test def sequences_separated_14_07 = test
  @Test def sequences_separated_14_08 = test

  // DAFFODIL-853 text number advanced props (V Symbol)
  @Ignore @Test def simple_type_properties_text_number_13_01 = test

  // DAFFODIL-840 textStandardBase (base 16)
  @Test def simple_type_properties_text_number_13_03 = test
}
