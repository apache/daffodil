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

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestRepType {
  val testDir = "/org/apache/daffodil/extensions/repType/"

  val runner = Runner(testDir, "repType.tdml", validateTDMLFile = false)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestRepType {
  import TestRepType._
  @Test def test_repType_keysetValue_00(): Unit = {
    runner.runOneTest("repType_keysetValue_00")
  }
  @Test def test_repType_keysetValue_01(): Unit = {
    runner.runOneTest("repType_keysetValue_01")
  }
  @Test def test_repType_keysetValue_02(): Unit = {
    runner.runOneTest("repType_keysetValue_02")
  }

  @Test def test_repType_unparse_keysetValue_00(): Unit = {
    runner.runOneTest("repType_unparse_keysetValue_00")
  }
  @Test def test_repType_unparse_keysetValue_01(): Unit = {
    runner.runOneTest("repType_unparse_keysetValue_01")
  }
  @Test def test_repType_unparse_keysetValue_02(): Unit = {
    runner.runOneTest("repType_unparse_keysetValue_02")
  }

  @Test def test_inherited_LengthKind(): Unit = { runner.runOneTest("inherited_LengthKind") }

  @Test def test_valueNotFound_1(): Unit = { runner.runOneTest("valueNotFound_1") }
  @Test def test_unparseValueNotFound_1(): Unit = {
    runner.runOneTest("unparseValueNotFound_1")
  }
  @Test def test_valueNotFound_2(): Unit = { runner.runOneTest("valueNotFound_2") }
  @Test def test_unparseValueNotFound_2(): Unit = {
    runner.runOneTest("unparseValueNotFound_2")
  }

  @Test def test_primitiveRep_invalid(): Unit = { runner.runOneTest("primitiveRep_invalid_01") }

  @Test def test_repType_overlap_01(): Unit = { runner.runOneTest("repType_overlap_01") }
  @Test def test_repType_bad_range_01(): Unit = { runner.runOneTest("repType_bad_range_01") }
  @Test def test_repType_odd_range_01(): Unit = { runner.runOneTest("repType_odd_range_01") }
  @Test def test_repType_complex_01(): Unit = { runner.runOneTest("repType_complex_01") }
  @Test def test_repType_no_restriction_01(): Unit = {
    runner.runOneTest("repType_no_restriction_01")
  }
  @Test def test_repType_no_enumerations_01(): Unit = {
    runner.runOneTest("repType_no_enumerations_01")
  }

  @Test def test_repType_immediate_01(): Unit = { runner.runOneTest("repType_immediate_01") }
  @Test def test_repType_immediate_02(): Unit = { runner.runOneTest("repType_immediate_02") }
  @Test def test_repType_indirection_01(): Unit = {
    runner.runOneTest("repType_indirection_01")
  }

  @Test def test_repType_length_facet_01(): Unit = {
    runner.runOneTest("repType_length_facet_01")
  }
  @Test def test_repType_length_facet_02(): Unit = {
    runner.runOneTest("repType_length_facet_02")
  }
  @Test def test_repType_length_facet_03(): Unit = {
    runner.runOneTest("repType_length_facet_03")
  }

  @Test def test_repType_negative_01(): Unit = { runner.runOneTest("repType_negative_01") }
  @Test def test_repType_negative_02(): Unit = { runner.runOneTest("repType_negative_02") }
  @Test def test_repType_negative_03(): Unit = { runner.runOneTest("repType_negative_03") }
  @Test def test_repType_negative_04(): Unit = { runner.runOneTest("repType_negative_04") }

  @Test def test_repType_hiddenGroup_01(): Unit = {
    runner.runOneTest("repType_hiddenGroup_01")
  }

  @Test def test_repType_different_namespaces_01(): Unit = {
    runner.runOneTest("repType_different_namespaces_01")
  }

  @Test def test_repValuesWithSpaces_01(): Unit = {
    runner.runOneTest("repValuesWithSpaces_01")
  }

  @Test def test_repValuesWithSpaces_02(): Unit = {
    runner.runOneTest("repValuesWithSpaces_02")
  }
}
