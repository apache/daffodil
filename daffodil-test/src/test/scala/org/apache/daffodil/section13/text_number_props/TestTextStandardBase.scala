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

package org.apache.daffodil.section13.text_standard_base

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestTextStandardBase extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/text_number_props/TextStandardBase.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

class TestTextStandardBase extends TdmlTests {
  val tdmlSuite = TestTextStandardBase

  // Tests of min, max, and 1 + max for each numeric data type in all non-base-10 bases

  @Test def base2_integer_min = test
  @Test def base8_integer_min = test
  @Test def base16_integer_min = test

  @Test def base2_integer_large = test
  @Test def base8_integer_large = test
  @Test def base16_integer_large = test

  @Test def base2_long_min = test
  @Test def base8_long_min = test
  @Test def base16_long_min = test

  @Test def base2_long_max = test
  @Test def base8_long_max = test
  @Test def base16_long_max = test

  @Test def base2_long_max_plus_one = test
  @Test def base8_long_max_plus_one = test
  @Test def base16_long_max_plus_one = test

  @Test def base2_int_min = test
  @Test def base8_int_min = test
  @Test def base16_int_min = test

  @Test def base2_int_max = test
  @Test def base8_int_max = test
  @Test def base16_int_max = test

  @Test def base2_int_max_plus_one = test
  @Test def base8_int_max_plus_one = test
  @Test def base16_int_max_plus_one = test

  @Test def base2_short_min = test
  @Test def base8_short_min = test
  @Test def base16_short_min = test

  @Test def base2_short_max = test
  @Test def base8_short_max = test
  @Test def base16_short_max = test

  @Test def base2_short_max_plus_one = test
  @Test def base8_short_max_plus_one = test
  @Test def base16_short_max_plus_one = test

  @Test def base2_byte_min = test
  @Test def base8_byte_min = test
  @Test def base16_byte_min = test

  @Test def base2_byte_max = test
  @Test def base8_byte_max = test
  @Test def base16_byte_max = test

  @Test def base2_byte_max_plus_one = test
  @Test def base8_byte_max_plus_one = test
  @Test def base16_byte_max_plus_one = test

  @Test def base2_uinteger_min = test
  @Test def base8_uinteger_min = test
  @Test def base16_uinteger_min = test

  @Test def base2_uinteger_large = test
  @Test def base8_uinteger_large = test
  @Test def base16_uinteger_large = test

  @Test def base2_ulong_min = test
  @Test def base8_ulong_min = test
  @Test def base16_ulong_min = test

  @Test def base2_ulong_max = test
  @Test def base8_ulong_max = test
  @Test def base16_ulong_max = test

  @Test def base2_ulong_max_plus_one = test
  @Test def base8_ulong_max_plus_one = test
  @Test def base16_ulong_max_plus_one = test

  @Test def base2_uint_min = test
  @Test def base8_uint_min = test
  @Test def base16_uint_min = test

  @Test def base2_uint_max = test
  @Test def base8_uint_max = test
  @Test def base16_uint_max = test

  @Test def base2_uint_max_plus_one = test
  @Test def base8_uint_max_plus_one = test
  @Test def base16_uint_max_plus_one = test

  @Test def base2_ushort_min = test
  @Test def base8_ushort_min = test
  @Test def base16_ushort_min = test

  @Test def base2_ushort_max = test
  @Test def base8_ushort_max = test
  @Test def base16_ushort_max = test

  @Test def base2_ushort_max_plus_one = test
  @Test def base8_ushort_max_plus_one = test
  @Test def base16_ushort_max_plus_one = test

  @Test def base2_ubyte_min = test
  @Test def base8_ubyte_min = test
  @Test def base16_ubyte_min = test

  @Test def base2_ubyte_max = test
  @Test def base8_ubyte_max = test
  @Test def base16_ubyte_max = test

  @Test def base2_ubyte_max_plus_one = test
  @Test def base8_ubyte_max_plus_one = test
  @Test def base16_ubyte_max_plus_one = test

  // SDE if type is float, double, or decimal in each non-base-10 base

  @Test def base2_float_err = test
  @Test def base8_float_err = test
  @Test def base16_float_err = test

  @Test def base2_double_err = test
  @Test def base8_double_err = test
  @Test def base16_double_err = test

  @Test def base2_decimal_err = test
  @Test def base8_decimal_err = test
  @Test def base16_decimal_err = test

  // PE if text number is the empty string

  @Test def non_base_10_empty_string_err = test

  // PE if leading sign

  @Test def non_base_10_leading_sign_negative_err = test
  @Test def non_base_10_leading_sign_positive_err = test

  // PE if text number contains characters not valid in the base

  @Test def base2_invalid_char_err = test
  @Test def base8_invalid_char_err = test
  @Test def base16_invalid_char_err = test

  // SDE if textStandardBase is not a supported base

  @Test def unsupported_base_err = test

  // Accepts uppercase hex characters, but always unparses to lowercase. Requires two-pass

  @Test def base16_uppercase = test

  // Leading zeros are accepted during parsing. Requires two-pass

  @Test def non_base_10_leading_zeros_ignored = test

  // SDE if unparsing a negative number

  @Test def non_base_10_unparse_negative_int_err = test
  @Test def non_base_10_unparse_negative_uinteger_err = test

  // Unparse always goes to lowercase

  @Test def non_base_10_unparse_lower_case = test
}
