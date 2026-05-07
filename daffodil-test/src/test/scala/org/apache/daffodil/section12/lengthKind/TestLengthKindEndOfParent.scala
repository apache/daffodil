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

import org.junit.Test

object TestLengthKindEndOfParent extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/EndOfParentTests.tdml"
}

class TestLengthKindEndOfParent extends TdmlTests {
  val tdmlSuite = TestLengthKindEndOfParent

  @Test def TestEndOfParentComplexTypesDelimited = test
  @Test def TestEndOfParentSimpleTypesDelimited = test
  @Test def TestEndOfParentComplexTypesImplicit = test
  @Test def TestEndOfParentSimpleTypesImplicit = test
  @Test def TestEndOfParentComplexTypesExplicit = test
  @Test def TestEndOfParentSimpleTypesExplicit = test
  @Test def TestEndOfParentCSVExplicit = test
  @Test def TestEndOfParentComplexTypesPrefixed = test
  @Test def TestEndOfParentSimpleTypesPrefixed = test
  @Test def TestEndOfParentComplexTypesPattern = test
  @Test def TestEndOfParentSimpleTypesPattern = test
  @Test def TestEndOfParentComplexTypesEOP = test
  @Test def TestEndOfParentSimpleTypesEOP = test
  @Test def TestEndOfParentComplexTypes1 = test
  @Test def TestEndOfParentSimpleTypes1 = test
  @Test def TestEndOfParentComplexTypes2 = test
  @Test def TestEndOfParentSimpleTypes2 = test
  @Test def TestEndOfParentComplexTypes3 = test
  @Test def TestEndOfParentSimpleTypes3 = test
  @Test def TestEndOfParentComplexTypes4 = test
  @Test def TestEndOfParentSimpleTypes4 = test
  @Test def TestEndOfParentComplexTypes5 = test
  @Test def TestEndOfParentSimpleTypes5 = test
  @Test def TestEndOfParentComplexTypes6 = test
  @Test def TestEndOfParentSimpleTypes6 = test
  @Test def TestEndOfParentComplexTypes7 = test
  @Test def TestEndOfParentSimpleTypes7 = test
  @Test def TestEndOfParentComplexTypes8 = test
  @Test def TestEndOfParentSimpleTypes8 = test
  @Test def TestEndOfParentComplexTypes9 = test
  @Test def TestEndOfParentSimpleTypes9 = test
  @Test def TestEndOfParentComplexTypes10 = test
  @Test def TestEndOfParentSimpleTypes10 = test
  @Test def TestEndOfParentComplexTypes11 = test
  @Test def TestEndOfParentSimpleTypes11 = test
//  @Test def TestEndOfParentComplexTypesRootChoice = test
//  @Test def TestEndOfParentSimpleTypesRootChoice = test
  @Test def TestEndOfParentComplexTypes12 = test
  @Test def TestEndOfParentSimpleTypes12 = test
  @Test def TestEndOfParentComplexTypes13 = test
  @Test def TestEndOfParentSimpleTypes13 = test
  @Test def TestEndOfParentSimpleTypes14 = test
  @Test def TestEndOfParentSimpleTypes16 = test
  @Test def TestEndOfParentSimpleTypes17 = test
  @Test def TestEndOfParentComplexTypesRootEOP = test
  @Test def TestEndOfParentSimpleTypesRootEOP = test

  @Test def text_string_txt_bytes = test
  @Test def text_string_txt_bits = test
  @Test def text_string_txt_chars = test
  @Test def text_string_txt_ref1 = test
  @Test def text_string_txt_ref2 = test
  @Test def text_string_txt_ref3 = test
  @Test def text_string_txt_bytes_nil = test
  @Test def text_string_txt_bits_nil = test
  @Test def text_string_txt_chars_nil = test
  @Test def text_int_txt_bytes = test
  @Test def text_int_txt_bytes_group_ref = test
  @Test def text_int_txt_bits = test
  @Test def text_int_txt_chars = test
  @Test def text_dec_txt_bytes = test
  @Test def text_dec_txt_bits = test
  @Test def text_dec_txt_chars = test
  @Test def text_date_txt_bytes = test
  @Test def text_date_txt_bits = test
  @Test def text_date_txt_chars = test
  @Test def text_bool_txt_bytes = test
  @Test def text_bool_txt_bits = test
  @Test def text_bool_txt_chars = test
  @Test def bin_int_bin_bytes_packed = test
  @Test def bin_int_bin_bytes_bcd = test
  @Test def bin_int_bin_bytes_ibm4690 = test
  @Test def bin_dec_bin_bytes = test
  @Test def bin_dec_bin_bytes_packed = test
  @Test def bin_dec_bin_bytes_bcd = test
  @Test def bin_dec_bin_bytes_ibm4690 = test
  @Test def bin_hex_bytes = test
  @Test def bin_bool_bin_bytes = test
  @Test def bin_date_bin_bytes_packed = test
  @Test def bin_date_bin_bytes_bcd = test
  @Test def bin_date_bin_bytes_ibm4690 = test

  @Test def nested_01 = test
  @Test def nested_02 = test
  @Test def nested_03 = test
  @Test def checks_01 = test
  @Test def checks_02 = test
}
