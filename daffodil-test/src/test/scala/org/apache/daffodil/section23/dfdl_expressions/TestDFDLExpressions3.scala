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

package org.apache.daffodil.section23.dfdl_expressions

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestDFDLExpressions3 {

  val testDir = "org/apache/daffodil/section23/dfdl_expressions/"
  val runner = Runner(testDir, "expressions3.tdml")

  @AfterClass def shutDown() = {
    runner.reset
  }
}

class TestDFDLExpressions3 {
  import TestDFDLExpressions3._

  // Fix to DAFFODIL-2192 removed the sharing of expression compilation across
  // multiple points of use. So these polymorphic situations no longer cause errors.
  //
  //  @Test def test_polymorphic_expr_1() { runner.runOneTest("test_polymorphic_expr_1") }
  //  @Test def test_polymorphic_expr_2a() { runner.runOneTest("test_polymorphic_expr_2a") }
  //  @Test def test_polymorphic_expr_2b() { runner.runOneTest("test_polymorphic_expr_2b") }
  //  @Test def test_polymorphic_expr_3() { runner.runOneTest("test_polymorphic_expr_3") }
  //  @Test def test_polymorphic_expr_4() { runner.runOneTest("test_polymorphic_expr_4") }
  //  @Test def test_polymorphic_expr_5() { runner.runOneTest("test_polymorphic_expr_5") }
  //  @Test def test_polymorphic_expr_6() { runner.runOneTest("test_polymorphic_expr_6") }

  // DAFFODIL-2182
  // @Test def test_array_self_expr1() { runner.runOneTest("test_array_self_expr1") }
  @Test def test_array_self_expr2(): Unit = { runner.runOneTest("test_array_self_expr2") }

  @Test def test_setVariable_neg_01(): Unit = { runner.runOneTest("setVariable_neg_01") }

  // DAFFODIL-2594
  // @Test def test_setVariable_neg_line_info_01(): Unit = { runner.runOneTest("setVariable_neg_line_info_01") }

  @Test def test_newVariableInstance_neg_01(): Unit = { runner.runOneTest("newVariableInstance_neg_01") }

  // DAFFODIL-2594
  // @Test def test_newVariableInstance_neg_line_info_01(): Unit = { runner.runOneTest("newVariableInstance_neg_line_info_01") }
}
