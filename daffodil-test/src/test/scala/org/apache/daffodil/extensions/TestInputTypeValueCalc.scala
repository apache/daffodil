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

object TestInputTypeValueCalc {
  val testDir = "/org/apache/daffodil/extensions/type_calc/"

  val runner = Runner(testDir, "inputTypeCalc.tdml", validateTDMLFile = false)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestInputTypeValueCalc {
  import TestInputTypeValueCalc._
  @Test def test_InputTypeCalc_keysetValue_00(): Unit = {
    runner.runOneTest("InputTypeCalc_keysetValue_00")
  }
  @Test def test_InputTypeCalc_keysetValue_01(): Unit = {
    runner.runOneTest("InputTypeCalc_keysetValue_01")
  }
  @Test def test_InputTypeCalc_keysetValue_02(): Unit = {
    runner.runOneTest("InputTypeCalc_keysetValue_02")
  }

  @Test def test_InputTypeCalc_unparse_keysetValue_00(): Unit = {
    runner.runOneTest("InputTypeCalc_unparse_keysetValue_00")
  }
  @Test def test_InputTypeCalc_unparse_keysetValue_01(): Unit = {
    runner.runOneTest("InputTypeCalc_unparse_keysetValue_01")
  }
  @Test def test_InputTypeCalc_unparse_keysetValue_02(): Unit = {
    runner.runOneTest("InputTypeCalc_unparse_keysetValue_02")
  }

  @Test def test_InputTypeCalc_unionOfKeysetValueCalcs_01(): Unit = {
    runner.runOneTest("InputTypeCalc_unionOfKeysetValueCalcs_01")
  }
  @Test def test_InputTypeCalc_unparse_unionOfKeysetValueCalcs_01(): Unit = {
    runner.runOneTest("InputTypeCalc_unparse_unionOfKeysetValueCalcs_01")
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
}
