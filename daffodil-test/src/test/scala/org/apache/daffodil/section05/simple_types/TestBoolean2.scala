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

package org.apache.daffodil.section05.simple_types

import org.apache.daffodil.tdml.Runner
import org.junit.Test
import org.junit.AfterClass

object TestBoolean2 {
  private val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "Boolean.tdml")

  @AfterClass def shutdown(): Unit = {
    runner.reset
  }
}

class TestBoolean2 {
  import TestBoolean2._

  @Test def test_binaryBoolean_0(): Unit = { runner.runOneTest("binaryBoolean_0") }
  @Test def test_binaryBoolean_unparse_0(): Unit = { runner.runOneTest("binaryBoolean_unparse_0") }
  @Test def test_binaryBoolean_1(): Unit = { runner.runOneTest("binaryBoolean_1") }
  @Test def test_binaryBoolean_unparse_1(): Unit = { runner.runOneTest("binaryBoolean_unparse_1") }
  @Test def test_binaryBoolean_unparse_2(): Unit = { runner.runOneTest("binaryBoolean_unparse_2") }
  @Test def test_binaryBoolean_2(): Unit = { runner.runOneTest("binaryBoolean_2") }
  @Test def test_binaryBoolean_pe_0(): Unit = { runner.runOneTest("binaryBoolean_pe_0") }
  @Test def test_binaryBoolean_sde_0(): Unit = { runner.runOneTest("binaryBoolean_sde_0") }
  @Test def test_binaryBoolean_sde_1(): Unit = { runner.runOneTest("binaryBoolean_sde_1") }
  @Test def test_textBoolean_0(): Unit = { runner.runOneTest("textBoolean_0") }
  @Test def test_textBoolean_0a(): Unit = { runner.runOneTest("textBoolean_0a") }
  @Test def test_textBoolean_unparse_0(): Unit = { runner.runOneTest("textBoolean_unparse_0") }
  @Test def test_textBoolean_1(): Unit = { runner.runOneTest("textBoolean_1") }
  @Test def test_textBoolean_2(): Unit = { runner.runOneTest("textBoolean_2") }
  @Test def test_textBoolean_3(): Unit = { runner.runOneTest("textBoolean_3") }
  @Test def test_textBoolean_sde_0(): Unit = { runner.runOneTest("textBoolean_sde_0") }
  @Test def test_textBoolean_sde_1(): Unit = { runner.runOneTest("textBoolean_sde_1") }
  @Test def test_textBoolean_sde_2(): Unit = { runner.runOneTest("textBoolean_sde_2") }
  @Test def test_textBoolean_unparse_sde_0(): Unit = { runner.runOneTest("textBoolean_unparse_sde_0") }
  @Test def test_textBoolean_sde_3(): Unit = { runner.runOneTest("textBoolean_sde_3") }
  @Test def test_textBoolean_sde_4(): Unit = { runner.runOneTest("textBoolean_sde_4") }
  @Test def test_textBoolean_sde_5(): Unit = { runner.runOneTest("textBoolean_sde_5") }
  @Test def test_textBoolean_pe_0(): Unit = { runner.runOneTest("textBoolean_pe_0") }
  @Test def test_textBoolean_unparseError(): Unit = { runner.runOneTest("textBoolean_unparseError") }

  @Test def test_textBoolean_IgnoreCase(): Unit = { runner.runOneTest("textBoolean_IgnoreCase") }
}
