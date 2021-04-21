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

package org.apache.daffodil.section12.lengthKind

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLengthKindPrefixed {
  private val testDir = "/org/apache/daffodil/section12/lengthKind/"

  val runner = Runner(testDir, "PrefixedTests.tdml", validateTDMLFile = false, validateDFDLSchemas = false)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestLengthKindPrefixed {

  import TestLengthKindPrefixed._

  @Test def test_pl_text_string_txt_bytes() = { runner.runOneTest("pl_text_string_txt_bytes") }
  @Test def test_pl_text_string_txt_bits() = { runner.runOneTest("pl_text_string_txt_bits") }
  @Test def test_pl_text_string_txt_chars() = { runner.runOneTest("pl_text_string_txt_chars") }
  @Test def test_pl_text_string_txt_bytes_includes() = { runner.runOneTest("pl_text_string_txt_bytes_includes") }
  @Test def test_pl_text_string_txt_bits_includes() = { runner.runOneTest("pl_text_string_txt_bits_includes") }
  @Test def test_pl_text_string_txt_chars_includes() = { runner.runOneTest("pl_text_string_txt_chars_includes") }
  @Test def test_pl_text_string_txt_chars_padding() = { runner.runOneTest("pl_text_string_txt_chars_padding") }
  @Test def test_pl_text_string_bin_bytes() = { runner.runOneTest("pl_text_string_bin_bytes") }
  @Test def test_pl_text_string_bin_bits() = { runner.runOneTest("pl_text_string_bin_bits") }
  @Test def test_pl_text_string_txt_bytes_nil() = { runner.runOneTest("pl_text_string_txt_bytes_nil") }
  @Test def test_pl_text_string_txt_bits_nil() = { runner.runOneTest("pl_text_string_txt_bits_nil") }
  @Test def test_pl_text_string_txt_chars_nil() = { runner.runOneTest("pl_text_string_txt_chars_nil") }
  @Test def test_pl_text_string_bin_bytes_nil() = { runner.runOneTest("pl_text_string_bin_bytes_nil") }
  @Test def test_pl_text_string_bin_bits_nil() = { runner.runOneTest("pl_text_string_bin_bits_nil") }
  @Test def test_pl_text_string_txt_bytes_neg_len() = { runner.runOneTest("pl_text_string_txt_bytes_neg_len") }
  @Test def test_pl_text_string_txt_bytes_not_enough_data() = { runner.runOneTest("pl_text_string_txt_bytes_not_enough_data") }
  @Test def test_pl_text_string_txt_bytes_not_enough_prefix_data() = { runner.runOneTest("pl_text_string_txt_bytes_not_enough_prefix_data") }
  //DFDL-2030, nested prefixed lengths not supported
  //@Test def test_pl_text_string_pl_txt_bytes() = { runner.runOneTest("pl_text_string_pl_txt_bytes") }
  @Test def test_pl_text_int_txt_bytes() = { runner.runOneTest("pl_text_int_txt_bytes") }
  @Test def test_pl_text_int_txt_bits() = { runner.runOneTest("pl_text_int_txt_bits") }
  @Test def test_pl_text_int_txt_chars() = { runner.runOneTest("pl_text_int_txt_chars") }
  @Test def test_pl_text_int_bin_bytes() = { runner.runOneTest("pl_text_int_bin_bytes") }
  @Test def test_pl_text_int_bin_bits() = { runner.runOneTest("pl_text_int_bin_bits") }
  @Test def test_pl_text_int_txt_bytes_includes() = { runner.runOneTest("pl_text_int_txt_bytes_includes") }
  @Test def test_pl_text_int_txt_bits_includes() = { runner.runOneTest("pl_text_int_txt_bits_includes") }
  @Test def test_pl_text_int_txt_chars_includes() = { runner.runOneTest("pl_text_int_txt_chars_includes") }
  @Test def test_pl_text_int_bin_bytes_includes() = { runner.runOneTest("pl_text_int_bin_bytes_includes") }
  @Test def test_pl_text_int_bin_bits_includes() = { runner.runOneTest("pl_text_int_bin_bits_includes") }
  @Test def test_pl_text_dec_txt_bytes() = { runner.runOneTest("pl_text_dec_txt_bytes") }
  @Test def test_pl_text_dec_txt_bits() = { runner.runOneTest("pl_text_dec_txt_bits") }
  @Test def test_pl_text_dec_txt_chars() = { runner.runOneTest("pl_text_dec_txt_chars") }
  @Test def test_pl_text_dec_bin_bytes() = { runner.runOneTest("pl_text_dec_bin_bytes") }
  @Test def test_pl_text_dec_bin_bits() = { runner.runOneTest("pl_text_dec_bin_bits") }
  @Test def test_pl_text_date_txt_bytes() = { runner.runOneTest("pl_text_date_txt_bytes") }
  @Test def test_pl_text_date_txt_bits() = { runner.runOneTest("pl_text_date_txt_bits") }
  @Test def test_pl_text_date_txt_chars() = { runner.runOneTest("pl_text_date_txt_chars") }
  @Test def test_pl_text_date_bin_bytes() = { runner.runOneTest("pl_text_date_bin_bytes") }
  @Test def test_pl_text_date_bin_bits() = { runner.runOneTest("pl_text_date_bin_bits") }
  @Test def test_pl_text_bool_txt_bytes() = { runner.runOneTest("pl_text_bool_txt_bytes") }
  @Test def test_pl_text_bool_txt_bits() = { runner.runOneTest("pl_text_bool_txt_bits") }
  @Test def test_pl_text_bool_txt_chars() = { runner.runOneTest("pl_text_bool_txt_chars") }
  @Test def test_pl_text_bool_bin_bytes() = { runner.runOneTest("pl_text_bool_bin_bytes") }
  @Test def test_pl_text_bool_bin_bits() = { runner.runOneTest("pl_text_bool_bin_bits") }

  @Test def test_pl_text_int_txt_bytes_plbits() = { runner.runOneTest("pl_text_int_txt_bytes_plbits") }
  @Test def test_pl_text_int_txt_bytes_plchars() = { runner.runOneTest("pl_text_int_txt_bytes_plchars") }
  @Test def test_pl_text_int_txt_bits_plbytes() = { runner.runOneTest("pl_text_int_txt_bits_plbytes") }
  @Test def test_pl_text_int_txt_bits_plchars() = { runner.runOneTest("pl_text_int_txt_bits_plchars") }
  @Test def test_pl_text_int_txt_chars_plbits() = { runner.runOneTest("pl_text_int_txt_chars_plbits") }
  @Test def test_pl_text_int_txt_chars_plbytes() = { runner.runOneTest("pl_text_int_txt_chars_plbytes") }
  @Test def test_pl_text_int_bin_bytes_plbits() = { runner.runOneTest("pl_text_int_bin_bytes_plbits") }
  @Test def test_pl_text_int_bin_bytes_plchars() = { runner.runOneTest("pl_text_int_bin_bytes_plchars") }
  @Test def test_pl_text_int_bin_bits_plbytes() = { runner.runOneTest("pl_text_int_bin_bits_plbytes") }
  @Test def test_pl_text_int_bin_bits_plchars() = { runner.runOneTest("pl_text_int_bin_bits_plchars") }

  @Test def test_pl_complex_bin_bytes() = { runner.runOneTest("pl_complex_bin_bytes") }
  @Test def test_pl_complex_bin_bits() = { runner.runOneTest("pl_complex_bin_bits") }
  @Test def test_pl_complex_bin_bytes_suspension() = { runner.runOneTest("pl_complex_bin_bytes_suspension") }
  @Test def test_pl_complex_bin_bytes_suspension_includes() = { runner.runOneTest("pl_complex_bin_bytes_suspension_includes") }
  @Test def test_pl_bin_int_txt_bytes() = { runner.runOneTest("pl_bin_int_txt_bytes") }
  @Test def test_pl_bin_int_txt_bits() = { runner.runOneTest("pl_bin_int_txt_bits") }
  @Test def test_pl_bin_int_bin_bytes() = { runner.runOneTest("pl_bin_int_bin_bytes") }
  @Test def test_pl_bin_int_bin_bits() = { runner.runOneTest("pl_bin_int_bin_bits") }
  @Test def test_pl_bin_int_txt_bytes_includes() = { runner.runOneTest("pl_bin_int_txt_bytes_includes") }
  @Test def test_pl_bin_int_txt_bits_includes() = { runner.runOneTest("pl_bin_int_txt_bits_includes") }
  @Test def test_pl_bin_int_bin_bytes_includes() = { runner.runOneTest("pl_bin_int_bin_bytes_includes") }
  @Test def test_pl_bin_int_bin_bits_includes() = { runner.runOneTest("pl_bin_int_bin_bits_includes") }
  @Test def test_pl_bin_int_bin_bytes_packed() = { runner.runOneTest("pl_bin_int_bin_bytes_packed") }
  @Test def test_pl_bin_int_bin_bits_packed() = { runner.runOneTest("pl_bin_int_bin_bits_packed") }
  @Test def test_pl_bin_int_bin_bytes_bcd() = { runner.runOneTest("pl_bin_int_bin_bytes_bcd") }
  @Test def test_pl_bin_int_bin_bits_bcd() = { runner.runOneTest("pl_bin_int_bin_bits_bcd") }
  @Test def test_pl_bin_int_bin_bytes_ibm4690() = { runner.runOneTest("pl_bin_int_bin_bytes_ibm4690") }
  @Test def test_pl_bin_int_bin_bits_ibm4690() = { runner.runOneTest("pl_bin_int_bin_bits_ibm4690") }
  @Test def test_pl_bin_dec_txt_bytes() = { runner.runOneTest("pl_bin_dec_txt_bytes") }
  @Test def test_pl_bin_dec_txt_bits() = { runner.runOneTest("pl_bin_dec_txt_bits") }
  @Test def test_pl_bin_dec_bin_bytes() = { runner.runOneTest("pl_bin_dec_bin_bytes") }
  @Test def test_pl_bin_dec_bin_bits() = { runner.runOneTest("pl_bin_dec_bin_bits") }
  @Test def test_pl_bin_dec_bin_bytes_packed() = { runner.runOneTest("pl_bin_dec_bin_bytes_packed") }
  @Test def test_pl_bin_dec_bin_bits_packed() = { runner.runOneTest("pl_bin_dec_bin_bits_packed") }
  @Test def test_pl_bin_dec_bin_bytes_bcd() = { runner.runOneTest("pl_bin_dec_bin_bytes_bcd") }
  @Test def test_pl_bin_dec_bin_bits_bcd() = { runner.runOneTest("pl_bin_dec_bin_bits_bcd") }
  @Test def test_pl_bin_dec_bin_bytes_ibm4690() = { runner.runOneTest("pl_bin_dec_bin_bytes_ibm4690") }
  @Test def test_pl_bin_dec_bin_bits_ibm4690() = { runner.runOneTest("pl_bin_dec_bin_bits_ibm4690") }
  @Test def test_pl_bin_hex_txt_bytes() = { runner.runOneTest("pl_bin_hex_txt_bytes") }
  @Test def test_pl_bin_hex_txt_bits() = { runner.runOneTest("pl_bin_hex_txt_bits") }
  @Test def test_pl_bin_hex_bin_bytes() = { runner.runOneTest("pl_bin_hex_bin_bytes") }
  @Test def test_pl_bin_hex_bin_bits() = { runner.runOneTest("pl_bin_hex_bin_bits") }
  @Test def test_pl_bin_bool_txt_bytes() = { runner.runOneTest("pl_bin_bool_txt_bytes") }
  @Test def test_pl_bin_bool_txt_bits() = { runner.runOneTest("pl_bin_bool_txt_bits") }
  @Test def test_pl_bin_bool_bin_bytes() = { runner.runOneTest("pl_bin_bool_bin_bytes") }
  @Test def test_pl_bin_bool_bin_bits() = { runner.runOneTest("pl_bin_bool_bin_bits") }
  @Test def test_pl_bin_date_bin_bytes_packed() = { runner.runOneTest("pl_bin_date_bin_bytes_packed") }
  @Test def test_pl_bin_date_bin_bits_packed() = { runner.runOneTest("pl_bin_date_bin_bits_packed") }
  @Test def test_pl_bin_date_bin_bytes_bcd() = { runner.runOneTest("pl_bin_date_bin_bytes_bcd") }
  @Test def test_pl_bin_date_bin_bits_bcd() = { runner.runOneTest("pl_bin_date_bin_bits_bcd") }
  @Test def test_pl_bin_date_bin_bytes_ibm4690() = { runner.runOneTest("pl_bin_date_bin_bytes_ibm4690") }
  @Test def test_pl_bin_date_bin_bits_ibm4690() = { runner.runOneTest("pl_bin_date_bin_bits_ibm4690") }
  @Test def test_plSlash1_data() = { runner.runOneTest("plSlash1_data") }

  @Test def test_pl_complex_err() = { runner.runOneTest("pl_complex_err") }
  @Test def test_pl_delimited_err() = { runner.runOneTest("pl_delimited_err") }
  @Test def test_pl_endofparent_err() = { runner.runOneTest("pl_endofparent_err") }
  @Test def test_pl_pattern_err() = { runner.runOneTest("pl_pattern_err") }
  @Test def test_pl_expression_err() = { runner.runOneTest("pl_expression_err") }
  @Test def test_pl_ovc_err() = { runner.runOneTest("pl_ovc_err") }
  @Test def test_pl_initiator_err() = { runner.runOneTest("pl_initiator_err") }
  @Test def test_pl_terminator_err() = { runner.runOneTest("pl_terminator_err") }
  @Test def test_pl_alignment_err() = { runner.runOneTest("pl_alignment_err") }
  @Test def test_pl_leadingskip_err() = { runner.runOneTest("pl_leadingskip_err") }
  @Test def test_pl_trailingskip_err() = { runner.runOneTest("pl_trailingskip_err") }
  @Test def test_pl_lengthunits_err() = { runner.runOneTest("pl_lengthunits_err") }
  @Test def test_pl_nest_err() = { runner.runOneTest("pl_nest_err") }
  @Test def test_pl_decimal_err() = { runner.runOneTest("pl_decimal_err") }
}
