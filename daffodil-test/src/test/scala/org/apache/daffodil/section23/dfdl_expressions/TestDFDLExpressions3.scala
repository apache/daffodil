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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestDFDLExpressions3 extends TdmlSuite {
  val tdmlResource = "org/apache/daffodil/section23/dfdl_expressions/expressions3.tdml"
}

class TestDFDLExpressions3 extends TdmlTests {
  val tdmlSuite = TestDFDLExpressions3

  // Fix to DAFFODIL-2192 removed the sharing of expression compilation across
  // multiple points of use. So these polymorphic situations no longer cause errors.
  @Ignore @Test def test_polymorphic_expr_1 = test
  @Ignore @Test def test_polymorphic_expr_2a = test
  @Ignore @Test def test_polymorphic_expr_2b = test
  @Ignore @Test def test_polymorphic_expr_3 = test
  @Ignore @Test def test_polymorphic_expr_4 = test
  @Ignore @Test def test_polymorphic_expr_5 = test
  @Ignore @Test def test_polymorphic_expr_6 = test

  @Test def test_array_self_expr1 = test
  @Test def test_array_self_expr2 = test
  @Test def test_array_path_expr1 = test
  @Test def test_array_path_expr2 = test
  @Test def test_array_path_expr3 = test

  @Test def setVariable_neg_01 = test

  // DAFFODIL-2594
  @Ignore @Test def setVariable_neg_line_info_01 = test

  @Test def newVariableInstance_neg_01 = test

  // DAFFODIL-2594
  @Ignore @Test def newVariableInstance_neg_line_info_01 = test
}
