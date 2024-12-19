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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestBoolean2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/Boolean.tdml"
}

class TestBoolean2 extends TdmlTests {
  val tdmlSuite = TestBoolean2

  @Test def binaryBoolean_0 = test
  @Test def binaryBoolean_unparse_0 = test
  @Test def binaryBoolean_1 = test
  @Test def binaryBoolean_unparse_1 = test
  @Test def binaryBoolean_unparse_2 = test
  @Test def binaryBoolean_2 = test
  @Test def binaryBoolean_pe_0 = test
  @Test def binaryBoolean_sde_0 = test
  @Test def binaryBoolean_sde_1 = test
  @Test def textBoolean_0 = test
  @Test def textBoolean_0a = test
  @Test def textBoolean_unparse_0 = test
  @Test def textBoolean_1 = test
  @Test def textBoolean_2 = test
  @Test def textBoolean_3 = test
  @Test def textBoolean_sde_0 = test
  @Test def textBoolean_sde_1 = test
  @Test def textBoolean_sde_2 = test
  @Test def textBoolean_unparse_sde_0 = test
  @Test def textBoolean_sde_3 = test
  @Test def textBoolean_sde_4 = test
  @Test def textBoolean_sde_5 = test
  @Test def textBoolean_pe_0 = test
  @Test def textBoolean_unparseError = test

  @Test def textBoolean_IgnoreCase = test
}
