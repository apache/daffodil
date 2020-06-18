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

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestTextStandardBase {
  val testDir = "/org/apache/daffodil/section13/text_number_props/"
  val runner = Runner(testDir, "TextStandardBase.tdml", validateTDMLFile = false, validateDFDLSchemas = false)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestTextStandardBase {
  import TestTextStandardBase._

  // Tests of min, max, and 1 + max for each numeric data type in all non-base-10 bases

  @Test def test_base2_integer_min(): Unit = { runner.runOneTest("base2_integer_min") }
  @Test def test_base8_integer_min(): Unit = { runner.runOneTest("base8_integer_min") }
  @Test def test_base16_integer_min(): Unit = { runner.runOneTest("base16_integer_min") }

  @Test def test_base2_integer_large(): Unit = { runner.runOneTest("base2_integer_large") }
  @Test def test_base8_integer_large(): Unit = { runner.runOneTest("base8_integer_large") }
  @Test def test_base16_integer_large(): Unit = { runner.runOneTest("base16_integer_large") }

  @Test def test_base2_long_min(): Unit = { runner.runOneTest("base2_long_min") }
  @Test def test_base8_long_min(): Unit = { runner.runOneTest("base8_long_min") }
  @Test def test_base16_long_min(): Unit = { runner.runOneTest("base16_long_min") }

  @Test def test_base2_long_max(): Unit = { runner.runOneTest("base2_long_max") }
  @Test def test_base8_long_max(): Unit = { runner.runOneTest("base8_long_max") }
  @Test def test_base16_long_max(): Unit = { runner.runOneTest("base16_long_max") }

  @Test def test_base2_long_max_plus_one(): Unit = { runner.runOneTest("base2_long_max_plus_one") }
  @Test def test_base8_long_max_plus_one(): Unit = { runner.runOneTest("base8_long_max_plus_one") }
  @Test def test_base16_long_max_plus_one(): Unit = { runner.runOneTest("base16_long_max_plus_one") }

  @Test def test_base2_int_min(): Unit = { runner.runOneTest("base2_int_min") }
  @Test def test_base8_int_min(): Unit = { runner.runOneTest("base8_int_min") }
  @Test def test_base16_int_min(): Unit = { runner.runOneTest("base16_int_min") }

  @Test def test_base2_int_max(): Unit = { runner.runOneTest("base2_int_max") }
  @Test def test_base8_int_max(): Unit = { runner.runOneTest("base8_int_max") }
  @Test def test_base16_int_max(): Unit = { runner.runOneTest("base16_int_max") }

  @Test def test_base2_int_max_plus_one(): Unit = { runner.runOneTest("base2_int_max_plus_one") }
  @Test def test_base8_int_max_plus_one(): Unit = { runner.runOneTest("base8_int_max_plus_one") }
  @Test def test_base16_int_max_plus_one(): Unit = { runner.runOneTest("base16_int_max_plus_one") }

  @Test def test_base2_short_min(): Unit = { runner.runOneTest("base2_short_min") }
  @Test def test_base8_short_min(): Unit = { runner.runOneTest("base8_short_min") }
  @Test def test_base16_short_min(): Unit = { runner.runOneTest("base16_short_min") }

  @Test def test_base2_short_max(): Unit = { runner.runOneTest("base2_short_max") }
  @Test def test_base8_short_max(): Unit = { runner.runOneTest("base8_short_max") }
  @Test def test_base16_short_max(): Unit = { runner.runOneTest("base16_short_max") }

  @Test def test_base2_short_max_plus_one(): Unit = { runner.runOneTest("base2_short_max_plus_one") }
  @Test def test_base8_short_max_plus_one(): Unit = { runner.runOneTest("base8_short_max_plus_one") }
  @Test def test_base16_short_max_plus_one(): Unit = { runner.runOneTest("base16_short_max_plus_one") }

  @Test def test_base2_byte_min(): Unit = { runner.runOneTest("base2_byte_min") }
  @Test def test_base8_byte_min(): Unit = { runner.runOneTest("base8_byte_min") }
  @Test def test_base16_byte_min(): Unit = { runner.runOneTest("base16_byte_min") }

  @Test def test_base2_byte_max(): Unit = { runner.runOneTest("base2_byte_max") }
  @Test def test_base8_byte_max(): Unit = { runner.runOneTest("base8_byte_max") }
  @Test def test_base16_byte_max(): Unit = { runner.runOneTest("base16_byte_max") }

  @Test def test_base2_byte_max_plus_one(): Unit = { runner.runOneTest("base2_byte_max_plus_one") }
  @Test def test_base8_byte_max_plus_one(): Unit = { runner.runOneTest("base8_byte_max_plus_one") }
  @Test def test_base16_byte_max_plus_one(): Unit = { runner.runOneTest("base16_byte_max_plus_one") }

  @Test def test_base2_uinteger_min(): Unit = { runner.runOneTest("base2_uinteger_min") }
  @Test def test_base8_uinteger_min(): Unit = { runner.runOneTest("base8_uinteger_min") }
  @Test def test_base16_uinteger_min(): Unit = { runner.runOneTest("base16_uinteger_min") }

  @Test def test_base2_uinteger_large(): Unit = { runner.runOneTest("base2_uinteger_large") }
  @Test def test_base8_uinteger_large(): Unit = { runner.runOneTest("base8_uinteger_large") }
  @Test def test_base16_uinteger_large(): Unit = { runner.runOneTest("base16_uinteger_large") }

  @Test def test_base2_ulong_min(): Unit = { runner.runOneTest("base2_ulong_min") }
  @Test def test_base8_ulong_min(): Unit = { runner.runOneTest("base8_ulong_min") }
  @Test def test_base16_ulong_min(): Unit = { runner.runOneTest("base16_ulong_min") }

  @Test def test_base2_ulong_max(): Unit = { runner.runOneTest("base2_ulong_max") }
  @Test def test_base8_ulong_max(): Unit = { runner.runOneTest("base8_ulong_max") }
  @Test def test_base16_ulong_max(): Unit = { runner.runOneTest("base16_ulong_max") }

  @Test def test_base2_ulong_max_plus_one(): Unit = { runner.runOneTest("base2_ulong_max_plus_one") }
  @Test def test_base8_ulong_max_plus_one(): Unit = { runner.runOneTest("base8_ulong_max_plus_one") }
  @Test def test_base16_ulong_max_plus_one(): Unit = { runner.runOneTest("base16_ulong_max_plus_one") }

  @Test def test_base2_uint_min(): Unit = { runner.runOneTest("base2_uint_min") }
  @Test def test_base8_uint_min(): Unit = { runner.runOneTest("base8_uint_min") }
  @Test def test_base16_uint_min(): Unit = { runner.runOneTest("base16_uint_min") }

  @Test def test_base2_uint_max(): Unit = { runner.runOneTest("base2_uint_max") }
  @Test def test_base8_uint_max(): Unit = { runner.runOneTest("base8_uint_max") }
  @Test def test_base16_uint_max(): Unit = { runner.runOneTest("base16_uint_max") }

  @Test def test_base2_uint_max_plus_one(): Unit = { runner.runOneTest("base2_uint_max_plus_one") }
  @Test def test_base8_uint_max_plus_one(): Unit = { runner.runOneTest("base8_uint_max_plus_one") }
  @Test def test_base16_uint_max_plus_one(): Unit = { runner.runOneTest("base16_uint_max_plus_one") }

  @Test def test_base2_ushort_min(): Unit = { runner.runOneTest("base2_ushort_min") }
  @Test def test_base8_ushort_min(): Unit = { runner.runOneTest("base8_ushort_min") }
  @Test def test_base16_ushort_min(): Unit = { runner.runOneTest("base16_ushort_min") }

  @Test def test_base2_ushort_max(): Unit = { runner.runOneTest("base2_ushort_max") }
  @Test def test_base8_ushort_max(): Unit = { runner.runOneTest("base8_ushort_max") }
  @Test def test_base16_ushort_max(): Unit = { runner.runOneTest("base16_ushort_max") }

  @Test def test_base2_ushort_max_plus_one(): Unit = { runner.runOneTest("base2_ushort_max_plus_one") }
  @Test def test_base8_ushort_max_plus_one(): Unit = { runner.runOneTest("base8_ushort_max_plus_one") }
  @Test def test_base16_ushort_max_plus_one(): Unit = { runner.runOneTest("base16_ushort_max_plus_one") }

  @Test def test_base2_ubyte_min(): Unit = { runner.runOneTest("base2_ubyte_min") }
  @Test def test_base8_ubyte_min(): Unit = { runner.runOneTest("base8_ubyte_min") }
  @Test def test_base16_ubyte_min(): Unit = { runner.runOneTest("base16_ubyte_min") }

  @Test def test_base2_ubyte_max(): Unit = { runner.runOneTest("base2_ubyte_max") }
  @Test def test_base8_ubyte_max(): Unit = { runner.runOneTest("base8_ubyte_max") }
  @Test def test_base16_ubyte_max(): Unit = { runner.runOneTest("base16_ubyte_max") }

  @Test def test_base2_ubyte_max_plus_one(): Unit = { runner.runOneTest("base2_ubyte_max_plus_one") }
  @Test def test_base8_ubyte_max_plus_one(): Unit = { runner.runOneTest("base8_ubyte_max_plus_one") }
  @Test def test_base16_ubyte_max_plus_one(): Unit = { runner.runOneTest("base16_ubyte_max_plus_one") }

  // SDE if type is float, double, or decimal in each non-base-10 base

  @Test def test_base2_float_err(): Unit = { runner.runOneTest("base2_float_err") }
  @Test def test_base8_float_err(): Unit = { runner.runOneTest("base8_float_err") }
  @Test def test_base16_float_err(): Unit = { runner.runOneTest("base16_float_err") }

  @Test def test_base2_double_err(): Unit = { runner.runOneTest("base2_double_err") }
  @Test def test_base8_double_err(): Unit = { runner.runOneTest("base8_double_err") }
  @Test def test_base16_double_err(): Unit = { runner.runOneTest("base16_double_err") }

  @Test def test_base2_decimal_err(): Unit = { runner.runOneTest("base2_decimal_err") }
  @Test def test_base8_decimal_err(): Unit = { runner.runOneTest("base8_decimal_err") }
  @Test def test_base16_decimal_err(): Unit = { runner.runOneTest("base16_decimal_err") }

  // PE if text number is the empty string

  @Test def test_non_base_10_empty_string_err(): Unit = { runner.runOneTest("non_base_10_empty_string_err") }

  // PE if leading sign

  @Test def test_non_base_10_leading_sign_negative_err(): Unit = { runner.runOneTest("non_base_10_leading_sign_negative_err") }
  @Test def test_non_base_10_leading_sign_positive_err(): Unit = { runner.runOneTest("non_base_10_leading_sign_positive_err") }

  // PE if text number contains characters not valid in the base

  @Test def test_base2_invalid_char_err(): Unit = { runner.runOneTest("base2_invalid_char_err") }
  @Test def test_base8_invalid_char_err(): Unit = { runner.runOneTest("base8_invalid_char_err") }
  @Test def test_base16_invalid_char_err(): Unit = { runner.runOneTest("base16_invalid_char_err") }

  // SDE if textStandardBase is not a supported base

  @Test def test_unsupported_base_err(): Unit = { runner.runOneTest("unsupported_base_err") }

  // Accepts uppercase hex characters, but always unparses to lowercase. Requires two-pass

  @Test def test_base16_uppercase(): Unit = { runner.runOneTest("base16_uppercase") }

  // Leading zeros are accepted during parsing. Requires two-pass

  @Test def test_non_base_10_leading_zeros_ignored(): Unit = { runner.runOneTest("non_base_10_leading_zeros_ignored") }

  // SDE if unparsing a negative number

  @Test def test_non_base_10_unparse_negative_int_err(): Unit = { runner.runOneTest("non_base_10_unparse_negative_int_err") }
  @Test def test_non_base_10_unparse_negative_uinteger_err(): Unit = { runner.runOneTest("non_base_10_unparse_negative_uinteger_err") }

  // Unparse always goes to lowercase

  @Test def test_non_base_10_unparse_lower_case(): Unit = { runner.runOneTest("non_base_10_unparse_lower_case") }

}
