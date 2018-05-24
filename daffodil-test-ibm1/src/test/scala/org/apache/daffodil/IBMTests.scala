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

import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.util.Misc
import org.junit.Test
import org.junit.Test

object IBMTestsThatPass {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1), validateTDMLFile = true, validateDFDLSchemas = false)
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
}

class IBMTestsThatPass {
  import IBMTestsThatPass._

  @Test def test_syntax_entities_6_04() { runner1.runOneTest("syntax_entities_6_04") }

  @Test def test_alignment_bytes_12_01() { runner1.runOneTest("alignment_bytes_12_01") }
  @Test def test_alignment_bytes_12_02() { runner1.runOneTest("alignment_bytes_12_02") }
  @Test def test_alignment_bytes_12_03() { runner1.runOneTest("alignment_bytes_12_03") }
  @Test def test_alignment_bytes_12_06() { runner1.runOneTest("alignment_bytes_12_06") }

  @Test def test_length_delimited_12_01() { runner1.runOneTest("length_delimited_12_01") }
  @Test def test_length_delimited_12_04() { runner1.runOneTest("length_delimited_12_04") }

  // Doesn't work for a user, possible locale issue (DAFFODIL-1945)
  // @Test def test_simple_type_properties_text_calendar_13_01() { runner2.runOneTest("simple_type_properties_text_calendar_13_01") }
  @Test def test_simple_type_properties_text_calendar_13_02() { runner2.runOneTest("simple_type_properties_text_calendar_13_02") }
  @Test def test_simple_type_properties_text_calendar_13_03() { runner2.runOneTest("simple_type_properties_text_calendar_13_03") }
  @Test def test_simple_type_properties_text_calendar_13_04() { runner2.runOneTest("simple_type_properties_text_calendar_13_04") }

  @Test def test_introduction_1_01() { runner1.runOneTest("introduction_1_01") }

  @Test def test_property_syntax_7_04() { runner1.runOneTest("property_syntax_7_04") }

  // Used to work, but now we get NYI for attributeFormDefault='qualified'
  // @Test def test_scoping_default_format_8_01() { runner1.runOneTest("scoping_default_format_8_01") }
  @Test def test_scoping_define_format_8_02() { runner1.runOneTest("scoping_define_format_8_02") }
  @Test def test_scoping_define_format_8_03() { runner1.runOneTest("scoping_define_format_8_03") }
  @Test def test_scoping_define_format_8_05() { runner1.runOneTest("scoping_define_format_8_05") }


  @Test def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }

  @Test def test_length_implicit_12_01() { runner1.runOneTest("length_implicit_12_01") }

  @Test def test_length_explicit_12_03() { runner1.runOneTest("length_explicit_12_03") }

  @Test def test_simple_type_properties_pad_trim_13_01() { runner2.runOneTest("simple_type_properties_pad_trim_13_01") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_02() { runner2.runOneTest("simple_type_properties_pad_trim_13_02") } // xs:integer type
  @Test def test_simple_type_properties_pad_trim_13_03() { runner2.runOneTest("simple_type_properties_pad_trim_13_03") } // pad/trim
  @Test def test_simple_type_properties_pad_trim_13_04() { runner2.runOneTest("simple_type_properties_pad_trim_13_04") } // pad/trim

  @Test def test_simple_type_properties_text_number_13_02() { runner2.runOneTest("simple_type_properties_text_number_13_02") }

  @Test def test_simple_type_properties_binary_number_13_01() { runner2.runOneTest("simple_type_properties_binary_number_13_01") }
  @Test def test_simple_type_properties_binary_number_13_02() { runner2.runOneTest("simple_type_properties_binary_number_13_02") }

  @Test def test_sequences_separated_14_01() { runner2.runOneTest("sequences_separated_14_01") }
  @Test def test_sequences_separated_14_02() { runner2.runOneTest("sequences_separated_14_02") }
  @Test def test_sequences_separated_14_07() { runner2.runOneTest("sequences_separated_14_07") }
  @Test def test_sequences_separated_14_08() { runner2.runOneTest("sequences_separated_14_08") }

  @Test def test_delimiter_12_02() { runner1.runOneTest("delimiter_12_02") }
}

