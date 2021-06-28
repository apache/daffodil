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

object IBMTestsThatPass2 {

  val runner1 = Runner("/test-suite/ibm-contributed/dpaext1.tdml")
  val runner2 = Runner("/test-suite/ibm-contributed/dpaext2.tdml")

  @AfterClass def shutDown(): Unit = {
    runner1.reset
    runner2.reset
  }
}

class IBMTestsThatPass2 {
  import IBMTestsThatPass2._

  @Test def test_simple_type_properties_text_boolean_13_01(): Unit = { runner2.runOneTest("simple_type_properties_text_boolean_13_01") } // DFDL-462 boolean type
  @Test def test_simple_type_properties_text_boolean_13_02(): Unit = { runner2.runOneTest("simple_type_properties_text_boolean_13_02") } // DFDL-462 boolean type

  @Test def test_alignment_bytes_12_04(): Unit = { runner1.runOneTest("alignment_bytes_12_04") } //DFDL-461 binary boolean

  @Test def test_schema_types_5_01(): Unit = { runner1.runOneTest("schema_types_5_01") }
  @Test def test_schema_types_5_02(): Unit = { runner1.runOneTest("schema_types_5_02") }
  @Test def test_schema_types_5_03(): Unit = { runner1.runOneTest("schema_types_5_03") }
  @Test def test_schema_types_5_04(): Unit = { runner1.runOneTest("schema_types_5_04") }
  @Test def test_schema_types_5_05(): Unit = { runner1.runOneTest("schema_types_5_05") }

  @Test def test_sequences_separated_14_03(): Unit = { runner2.runOneTest("sequences_separated_14_03") }
  @Test def test_sequences_separated_14_05(): Unit = { runner2.runOneTest("sequences_separated_14_05") }
  @Test def test_sequences_separated_14_06(): Unit = { runner2.runOneTest("sequences_separated_14_06") }

  @Test def test_multiple_delimiters2(): Unit = { runner1.runOneTest("multiple_delimiters2") }
  @Test def test_multiple_delimiters2err(): Unit = { runner1.runOneTest("multiple_delimiters2_err") }

  // Fails on IBM DFDL
  // @Test def test_multiple_delimiters2_ibm() { runner1.runOneTest("multiple_delimiters2_ibm") }

  @Test def test_syntax_entities_6_01(): Unit = { runner1.runOneTest("syntax_entities_6_01") }
  @Test def test_syntax_entities_6_02(): Unit = { runner1.runOneTest("syntax_entities_6_02") }
  //
  // Needs DFDL-1559 - CR;LF output
  //  @Test def test_syntax_entities_6_03() { runner1.runOneTest("syntax_entities_6_03") }

  @Test def test_property_syntax_7_01(): Unit = { runner1.runOneTest("property_syntax_7_01") }
  @Test def test_property_syntax_7_02(): Unit = { runner1.runOneTest("property_syntax_7_02") }
  @Test def test_property_syntax_7_03(): Unit = { runner1.runOneTest("property_syntax_7_03") }

  @Test def test_choices_basic_15_01(): Unit = { runner2.runOneTest("choices_basic_15_01") }
  @Test def test_choices_basic_15_02(): Unit = { runner2.runOneTest("choices_basic_15_02") }
  @Test def test_choices_basic_15_03(): Unit = { runner2.runOneTest("choices_basic_15_03") }

  @Test def test_arrays_16_01(): Unit = { runner2.runOneTest("arrays_16_01") }

  @Test def test_introduction_1_02(): Unit = { runner1.runOneTest("introduction_1_02") }
  @Test def test_length_delimited_12_03(): Unit = { runner1.runOneTest("length_delimited_12_03") }
  @Test def test_length_delimited_12_02(): Unit = { runner1.runOneTest("length_delimited_12_02") }
  @Test def test_multiple_delimiters(): Unit = { runner1.runOneTest("multiple_delimiters") }

  @Test def test_encoding_11_01(): Unit = { runner1.runOneTest("encoding_11_01") }
  @Test def test_encoding_11_02(): Unit = { runner1.runOneTest("encoding_11_02") }
  @Test def test_encoding_11_03(): Unit = { runner1.runOneTest("encoding_11_03") }

  @Test def test_delimiter_12_01(): Unit = { runner1.runOneTest("delimiter_12_01") }
  @Test def test_delimiter_12_02(): Unit = { runner1.runOneTest("delimiter_12_02") }
  @Test def test_delimiter_12_03(): Unit = { runner1.runOneTest("delimiter_12_03") }
  @Test def test_delimiter_12_04(): Unit = { runner1.runOneTest("delimiter_12_04") }

  // uses lengthUnits bytes with lengthKind explicit and utf-8
  @Test def test_length_explicit_12_01(): Unit = { runner1.runOneTest("length_explicit_12_01") }
  @Test def test_length_explicit_12_02(): Unit = { runner1.runOneTest("length_explicit_12_02") }
  @Test def test_length_delimited_12_06(): Unit = { runner1.runOneTest("length_delimited_12_06") }

  @Test def test_alignment_bytes_12_05(): Unit = { runner1.runOneTest("alignment_bytes_12_05") } //DFDL-99 binary dateTime

  @Test def test_length_implicit_12_02(): Unit = { runner1.runOneTest("length_implicit_12_02") } // implicit length string - bug in IBM test (doesn't have minLength - both are required)
  @Test def test_simple_type_properties_text_boolean_13_03(): Unit = { runner2.runOneTest("simple_type_properties_text_boolean_13_03") } // DFDL-462 boolean type
  @Test def test_simple_type_properties_bin_boolean_13_01(): Unit = { runner2.runOneTest("simple_type_properties_bin_boolean_13_01") } // DFDL-461 boolean type

  @Test def test_sequences_separated_14_04(): Unit = { runner2.runOneTest("sequences_separated_14_04") } // left over data

}
