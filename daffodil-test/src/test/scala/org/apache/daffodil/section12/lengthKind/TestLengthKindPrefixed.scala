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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestLengthKindPrefixed extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/PrefixedTests.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

class TestLengthKindPrefixed extends TdmlTests {
  val tdmlSuite = TestLengthKindPrefixed

  @Test def pl_text_string_txt_bytes = test
  @Test def pl_text_string_txt_bits = test
  @Test def pl_text_string_txt_chars = test
  @Test def pl_text_string_txt_bytes_includes = test
  @Test def pl_text_string_txt_bits_includes = test
  @Test def pl_text_string_txt_chars_includes = test
  @Test def pl_text_string_txt_chars_padding = test
  @Test def pl_text_string_bin_bytes = test
  @Test def pl_text_string_bin_bits = test
  @Test def pl_text_string_txt_bytes_nil = test
  @Test def pl_text_string_txt_bits_nil = test
  @Test def pl_text_string_txt_chars_nil = test
  @Test def pl_text_string_bin_bytes_nil = test
  @Test def pl_text_string_bin_bits_nil = test
  @Test def pl_text_string_txt_bytes_neg_len = test
  @Test def pl_text_string_txt_bytes_not_enough_data = test
  @Test def pl_text_string_txt_bytes_not_enough_prefix_data = test
  @Test def pl_text_string_txt_bytes_not_enough_prefix_data_includes_backtrack = test
  // DFDL-2660
  @Test def pl_check_prefix_facets_before_use1 = test
  @Test def pl_check_prefix_facets_before_use2 = test
  @Test def pl_check_prefix_facets_before_use3 = test
  @Test def pl_check_prefix_facets_before_use4 = test
  @Test def pl_check_prefix_facets_before_use5 = test
  @Test def pl_check_prefix_facets_before_use6 = test
  @Test def pl_check_prefix_facets_before_use7 = test
  @Test def pl_check_prefix_facets_before_use8 = test
  @Test def pl_check_prefix_facets_before_use9 = test
  @Test def pl_check_prefix_for_annotations = test
  // DFDL-2030, nested prefixed lengths not supported
  @Ignore @Test def pl_text_string_pl_txt_bytes = test
  @Test def pl_text_int_txt_bytes = test
  @Test def pl_text_int_txt_bits = test
  @Test def pl_text_int_txt_chars = test
  @Test def pl_text_int_bin_bytes = test
  @Test def pl_text_int_bin_bits = test
  @Test def pl_text_int_txt_bytes_includes = test
  @Test def pl_text_int_txt_bits_includes = test
  @Test def pl_text_int_txt_chars_includes = test
  @Test def pl_text_int_bin_bytes_includes = test
  @Test def pl_text_int_bin_bits_includes = test
  @Test def pl_text_dec_txt_bytes = test
  @Test def pl_text_dec_txt_bits = test
  @Test def pl_text_dec_txt_chars = test
  @Test def pl_text_dec_bin_bytes = test
  @Test def pl_text_dec_bin_bits = test
  @Test def pl_text_date_txt_bytes = test
  @Test def pl_text_date_txt_bits = test
  @Test def pl_text_date_txt_chars = test
  @Test def pl_text_date_bin_bytes = test
  @Test def pl_text_date_bin_bits = test
  @Test def pl_text_bool_txt_bytes = test
  @Test def pl_text_bool_txt_bits = test
  @Test def pl_text_bool_txt_chars = test
  @Test def pl_text_bool_bin_bytes = test
  @Test def pl_text_bool_bin_bits = test

  @Test def pl_text_int_txt_bytes_plbits = test
  @Test def pl_text_int_txt_bytes_plchars = test
  @Test def pl_text_int_txt_bits_plbytes = test
  @Test def pl_text_int_txt_bits_plchars = test
  @Test def pl_text_int_txt_chars_plbits = test
  @Test def pl_text_int_txt_chars_plbytes = test
  @Test def pl_text_int_bin_bytes_plbits = test
  @Test def pl_text_int_bin_bytes_plchars = test
  @Test def pl_text_int_bin_bits_plbytes = test
  @Test def pl_text_int_bin_bits_plchars = test

  @Test def pl_complex_bin_bytes = test
  @Test def pl_complex_bin_bits = test
  @Test def pl_complex_bin_bytes_suspension = test
  @Test def pl_complex_bin_bytes_suspension_includes = test
  @Test def pl_bin_int_txt_bytes = test
  @Test def pl_bin_int_txt_bits = test
  @Test def pl_bin_int_bin_bytes = test
  @Test def pl_bin_int_bin_bits = test
  @Test def pl_bin_int_txt_bytes_includes = test
  @Test def pl_bin_int_txt_bits_includes = test
  @Test def pl_bin_int_bin_bytes_includes = test
  @Test def pl_bin_int_bin_bits_includes = test
  @Test def pl_bin_int_bin_bytes_packed = test
  @Test def pl_bin_int_bin_bits_packed = test
  @Test def pl_bin_int_bin_bytes_bcd = test
  @Test def pl_bin_int_bin_bits_bcd = test
  @Test def pl_bin_int_bin_bytes_ibm4690 = test
  @Test def pl_bin_int_bin_bits_ibm4690 = test
  @Test def pl_bin_dec_txt_bytes = test
  @Test def pl_bin_dec_txt_bits = test
  @Test def pl_bin_dec_bin_bytes = test
  @Test def pl_bin_dec_bin_bits = test
  @Test def pl_bin_dec_bin_bytes_packed = test
  @Test def pl_bin_dec_bin_bits_packed = test
  @Test def pl_bin_dec_bin_bytes_bcd = test
  @Test def pl_bin_dec_bin_bits_bcd = test
  @Test def pl_bin_dec_bin_bytes_ibm4690 = test
  @Test def pl_bin_dec_bin_bits_ibm4690 = test
  @Test def pl_bin_hex_txt_bytes = test
  @Test def pl_bin_hex_txt_bits = test
  @Test def pl_bin_hex_bin_bytes = test
  @Test def pl_bin_hex_bin_bits = test
  @Test def pl_bin_bool_txt_bytes = test
  @Test def pl_bin_bool_txt_bits = test
  @Test def pl_bin_bool_bin_bytes = test
  @Test def pl_bin_bool_bin_bits = test
  @Test def pl_bin_date_bin_bytes_packed = test
  @Test def pl_bin_date_bin_bits_packed = test
  @Test def pl_bin_date_bin_bytes_bcd = test
  @Test def pl_bin_date_bin_bits_bcd = test
  @Test def pl_bin_date_bin_bytes_ibm4690 = test
  @Test def pl_bin_date_bin_bits_ibm4690 = test
  @Test def plSlash1_data = test

  @Test def pl_complex_err = test
  @Test def pl_delimited_err = test
  @Test def pl_endofparent_err = test
  @Test def pl_pattern_err = test
  @Test def pl_expression_err = test
  @Test def pl_ovc_err = test
  @Test def pl_initiator_err = test
  @Test def pl_terminator_err = test
  @Test def pl_alignment_err = test
  @Test def pl_leadingskip_err = test
  @Test def pl_trailingskip_err = test
  @Test def pl_lengthunits_err = test
  @Test def pl_nest_err = test
  @Test def pl_decimal_err = test

  // DAFFODIL-2657
  @Test def pl_implicit_1 = test

  // DAFFODIL-2656
  @Test def pl_complexContentLengthBytes_1 = test
  @Test def pl_complexValueLengthBytes_1 = test
  @Test def pl_complexContentLengthBits_1 = test
  @Test def pl_complexValueLengthBits_1 = test
  @Test def pl_simpleContentLengthBytes_1 = test

  // DAFFODIL-2658
  @Ignore @Test def pl_simpleValueLengthBytes_1 = test

  @Test def pl_simpleContentLengthCharacters_1 = test
  @Test def pl_complexContentLengthCharacters_1 = test
  @Test def pl_complexContentLengthCharacters_utf8_1 = test
  @Test def lengthUnitsBitsForNonNegativeInteger_prefixed = test

  @Test def invalidUnsignedLongBitLength = test
  @Test def invalidUnsignedLongByteLength = test
  @Test def invalidUnsignedIntBitLength = test
  @Test def invalidUnsignedShortBitLength = test
  @Test def invalidUnsignedByteBitLength = test

  @Test def invalidLongBitLength = test
  @Test def invalidIntBitLength = test
  @Test def invalidShortBitLength = test
  @Test def invalidByteBitLength = test
}
