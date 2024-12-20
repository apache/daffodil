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

package org.apache.daffodil.section14.unordered_sequences

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestUnorderedSequences extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section14/unordered_sequences/UnorderedSequences.tdml"
}

object TestBE extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section14/unordered_sequences/BE.tdml"
}

class TestUnorderedSequences extends TdmlTests {
  val tdmlSuite = TestUnorderedSequences

  // DFDL-1010
  @Test def test_simple = test
  @Test def test_simple_fail_scalar = test
  @Test def test_simple_min_max_occurs = test
  @Test def test_simple_min_max_occurs_fail = test
  @Test def test_array_reference = test
  @Test def test_simple_delimited = test
  @Test def test_separated_infix = test
  @Test def test_separated_prefix = test
  @Test def test_separated_postfix = test
  @Test def test_simple_nil = test
  @Test def test_simple_optional_elem = test
  @Test def test_simple_invalid_path_to_branch = test
  @Test def test_simple_invalid_path_to_branch_does_not_exist = test
  @Test def test_nested_valid_path_to_branch = test
  @Test def test_nested_multiple_valid_paths_to_branch = test
  @Test def test_nested_multiple_invalid_paths_to_branch = test
  @Test def test_sde_element_element_ref = test
  @Test def test_sde_optional_array_ock_parsed = test
  @Test def test_sde_unique_names_in_ns = test

  @Test def test_empty_seq = test

  @Test def test_initiated_unordered1 = test
  @Test def test_initiated_unordered2 = test
  @Test def test_initiated_unordered3 = test

  @Test def test_unordered_namespaces_01 = test
}

class TestBE extends TdmlTests {
  val tdmlSuite = TestBE

  @Test def BE000 = test
  @Test def BE001 = test
  @Test def BE002 = test
  @Test def BE003 = test
  @Test def BE004 = test
  @Test def BE004_A = test
}
