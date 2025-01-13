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

package org.apache.daffodil.extensions

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestRepType extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/extensions/repType/repType.tdml"
}

class TestRepType extends TdmlTests {
  val tdmlSuite = TestRepType

  @Test def repType_keysetValue_00 = test
  @Test def repType_keysetValue_01 = test
  @Test def repType_keysetValue_02 = test

  @Test def repType_unparse_keysetValue_00 = test
  @Test def repType_unparse_keysetValue_01 = test
  @Test def repType_unparse_keysetValue_02 = test

  @Test def inherited_LengthKind = test

  @Test def valueNotFound_1 = test
  @Test def unparseValueNotFound_1 = test
  @Test def valueNotFound_2 = test
  @Test def unparseValueNotFound_2 = test

  @Test def primitiveRep_invalid_01 = test

  @Test def repType_overlap_01 = test
  @Test def repType_bad_range_01 = test
  @Test def repType_odd_range_01 = test
  @Test def repType_complex_01 = test
  @Test def repType_no_restriction_01 = test
  @Test def repType_no_enumerations_01 = test

  @Test def repType_immediate_01 = test
  @Test def repType_immediate_02 = test
  @Test def repType_indirection_01 = test

  @Test def repType_length_facet_01 = test
  @Test def repType_length_facet_02 = test
  @Test def repType_length_facet_03 = test
  @Test def repType_length_facet_04 = test
  @Test def repType_length_facet_05 = test
  @Test def repType_length_facet_06 = test

  @Test def repType_negative_01 = test
  @Test def repType_negative_02 = test
  @Test def repType_negative_03 = test
  @Test def repType_negative_04 = test

  @Test def repType_hiddenGroup_01 = test

  @Test def repType_different_namespaces_01 = test

  @Test def repValuesWithSpaces_01 = test

  @Test def repValuesWithSpaces_02 = test
}
