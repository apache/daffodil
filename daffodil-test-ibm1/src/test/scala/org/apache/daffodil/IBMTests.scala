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

import org.apache.daffodil.tdml.Runner
import org.junit.Test
import org.junit.AfterClass

object IBMTestsThatPass {
  val runner1 = Runner("/test-suite/ibm-contributed/", "dpaext1.tdml", validateTDMLFile = true, validateDFDLSchemas = false)
  val runner2 = Runner("/test-suite/ibm-contributed/dpaext2.tdml")

  @AfterClass def tearDown(): Unit = {
    runner1.reset
    runner2.reset
  }
}

class IBMTestsThatPass {
  import IBMTestsThatPass._

  @Test def test_syntax_entities_6_04(): Unit = { runner1.runOneTest("syntax_entities_6_04") }

  @Test def test_alignment_bytes_12_01(): Unit = { runner1.runOneTest("alignment_bytes_12_01") }
  @Test def test_alignment_bytes_12_02(): Unit = { runner1.runOneTest("alignment_bytes_12_02") }
  @Test def test_alignment_bytes_12_03(): Unit = { runner1.runOneTest("alignment_bytes_12_03") }
  @Test def test_alignment_bytes_12_06(): Unit = { runner1.runOneTest("alignment_bytes_12_06") }

  @Test def test_length_delimited_12_01(): Unit = { runner1.runOneTest("length_delimited_12_01") }
  @Test def test_length_delimited_12_04(): Unit = { runner1.runOneTest("length_delimited_12_04") }

  @Test def test_simple_type_properties_text_calendar_13_01(): Unit = { runner2.runOneTest("simple_type_properties_text_calendar_13_01") }
  @Test def test_simple_type_properties_text_calendar_13_02(): Unit = { runner2.runOneTest("simple_type_properties_text_calendar_13_02") }
  @Test def test_simple_type_properties_text_calendar_13_03(): Unit = { runner2.runOneTest("simple_type_properties_text_calendar_13_03") }
  @Test def test_simple_type_properties_text_calendar_13_04(): Unit = { runner2.runOneTest("simple_type_properties_text_calendar_13_04") }

  @Test def test_simple_type_properties_bin_calendar_13_01(): Unit = { runner2.runOneTest("simple_type_properties_bin_calendar_13_01") }
  @Test def test_simple_type_properties_bin_calendar_13_02(): Unit = { runner2.runOneTest("simple_type_properties_bin_calendar_13_02") }

  @Test def test_introduction_1_01(): Unit = { runner1.runOneTest("introduction_1_01") }

  @Test def test_property_syntax_7_04(): Unit = { runner1.runOneTest("property_syntax_7_04") }

  @Test def test_scoping_default_format_8_01(): Unit = { runner1.runOneTest("scoping_default_format_8_01") }
  @Test def test_scoping_define_format_8_01(): Unit = { runner1.runOneTest("scoping_define_format_8_01") }
  @Test def test_scoping_define_format_8_02(): Unit = { runner1.runOneTest("scoping_define_format_8_02") }
  @Test def test_scoping_define_format_8_03(): Unit = { runner1.runOneTest("scoping_define_format_8_03") }
  @Test def test_scoping_define_format_8_04(): Unit = { runner1.runOneTest("scoping_define_format_8_04") }
  @Test def test_scoping_define_format_8_05(): Unit = { runner1.runOneTest("scoping_define_format_8_05") }

  @Test def test_encoding_11_01(): Unit = { runner1.runOneTest("encoding_11_01") }

  @Test def test_length_implicit_12_01(): Unit = { runner1.runOneTest("length_implicit_12_01") }

  @Test def test_length_explicit_12_03(): Unit = { runner1.runOneTest("length_explicit_12_03") }

  @Test def test_simple_type_properties_pad_trim_13_01(): Unit = { runner2.runOneTest("simple_type_properties_pad_trim_13_01") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_02(): Unit = { runner2.runOneTest("simple_type_properties_pad_trim_13_02") } // xs:integer type
  @Test def test_simple_type_properties_pad_trim_13_03(): Unit = { runner2.runOneTest("simple_type_properties_pad_trim_13_03") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_04(): Unit = { runner2.runOneTest("simple_type_properties_pad_trim_13_04") } // pad/trim

  @Test def test_simple_type_properties_text_number_13_02(): Unit = { runner2.runOneTest("simple_type_properties_text_number_13_02") }

  @Test def test_simple_type_properties_binary_number_13_01(): Unit = { runner2.runOneTest("simple_type_properties_binary_number_13_01") }
  @Test def test_simple_type_properties_binary_number_13_02(): Unit = { runner2.runOneTest("simple_type_properties_binary_number_13_02") }

  @Test def test_sequences_separated_14_01(): Unit = { runner2.runOneTest("sequences_separated_14_01") }
  @Test def test_sequences_separated_14_02(): Unit = { runner2.runOneTest("sequences_separated_14_02") }
  @Test def test_sequences_separated_14_07(): Unit = { runner2.runOneTest("sequences_separated_14_07") }
  @Test def test_sequences_separated_14_08(): Unit = { runner2.runOneTest("sequences_separated_14_08") }

  @Test def test_delimiter_12_02(): Unit = { runner1.runOneTest("delimiter_12_02") }

  // DAFFODIL-1541 Need support for handling delimited data with encoding other than ISO-8859-1
  //@Test def test_length_delimited_12_05() { runner1.runOneTest("length_delimited_12_05") }

  // DAFFODIL-853 text number advanced props (V Symbol)
  //@Test def test_simple_type_properties_text_number_13_01() { runner2.runOneTest("simple_type_properties_text_number_13_01") }

  // DAFFODIL-840 textStandardBase (base 16)
  @Test def test_simple_type_properties_text_number_13_03(): Unit = { runner2.runOneTest("simple_type_properties_text_number_13_03") }

  // DAFFODIL-551 Needs dfdl:utf16Width='variable' implementation
  //@Test def test_syntax_entities_6_03() { runner1.runOneTest("syntax_entities_6_03") }
}
