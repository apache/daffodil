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

package org.apache.daffodil.usertests

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestSepTests extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/usertests/SepTests.tdml"
}

class TestSepTests extends TdmlTests {

  val tdmlSuite = TestSepTests

  @Test def test_sep_trailing_1 = test
  @Test def test_sep_anyEmpty_1 = test
  // DAFFODIL-2498 anyEmpty with minOccurs '0', and empty as first occurrence.
  @Test def test_sep_anyEmpty_2 = test

  @Test def test_sep_trailingEmptyStrict_1 = test
  @Test def test_sep_trailingEmptyStrict_2 = test

  @Test def test_sep_ssp_never_1 = test
  @Test def test_sep_ssp_never_2 = test
  @Test def test_sep_ssp_never_3 = test
  @Test def test_sep_ssp_never_4_ibm = test
  @Test def test_sep_ssp_never_4_daffodil = test
  @Test def test_sep_ssp_never_5 = test

  @Test def test_sep_ssp_never_6 = test
  @Test def test_sep_ssp_never_7 = test

  // DAFFODIL-3094
  @Test def test_sep_ssp_never_8 = test
  @Test def test_sep_ssp_never_9 = test
  @Test def test_sep_unparse_positional_implicit_array_extra_seps = test
  @Test def test_sep_parse_discriminated_group = test
  @Test def test_sep_ssp_never_10 = test
  @Test def test_sep_ssp_never_11 = test
  @Test def test_sep_ssp_never_12 = test

  @Test def test_sep_ssp_trailing_1 = test
  @Test def test_sep_ssp_trailing_2 = test
  @Test def test_sep_ssp_trailing_3 = test
  @Test def test_sep_ssp_trailing_4 = test

  @Test def test_sep_ssp_never_13 = test
  @Test def test_sep_ssp_never_14 = test
  @Test def test_sep_ssp_never_15 = test
  @Test def test_sep_ssp_never_16 = test

  @Test def test_sep_ssp_any_1 = test
  @Test def test_sep_ssp_any_2 = test
  @Test def test_sep_ssp_any_3 = test
  @Test def test_sep_ssp_any_4 = test

  @Test def test_sep_ssp_strict_1 = test
  @Test def test_sep_ssp_strict_2 = test
  @Test def test_sep_ssp_strict_3 = test
  @Test def test_sep_ssp_strict_4 = test

  // DAFFODIL-2205 - EmptyValueDelimiterPolicy only works with 'both'
  @Test def test_sep_evdp_1 = test
  @Ignore @Test def test_sep_evdp_2 = test

  // DAFFODIL-2791
  @Test def test_treatAsAbsent_occursIndex = test

  // DAFFODIL-2295
  @Test def test_sep_alignment_1 = test
  @Test def test_sep_alignment_2 = test
  @Test def test_sep_alignment_3 = test

  @Test def test_sep_alignment_4 = test
}
