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

object TestSimpleTypesUnparse extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/SimpleTypesUnparse.tdml"
}

class TestSimpleTypesUnparse extends TdmlTests {
  val tdmlSuite = TestSimpleTypesUnparse

  @Test def hexBinary_unparse_01 = test
  @Test def hexBinary_unparse_02 = test
  @Test def hexBinary_unparse_03 = test
  @Test def hexBinary_unparse_04 = test
  @Test def hexBinary_unparse_05 = test
  @Test def hexBinary_unparse_06 = test
  @Test def hexBinary_unparse_07 = test
  @Test def hexBinary_unparse_08 = test
  @Test def hexBinary_unparse_09 = test
  @Test def hexBinary_unparse_10 = test
  @Test def hexBinary_unparse_11 = test
  @Test def hexBinary_unparse_12 = test
  @Test def hexBinary_unparse_13 = test
  @Test def hexBinary_unparse_14 = test

  @Test def hexBinary_unparse_15 = test
  @Test def hexBinary_unparse_16 = test
  @Test def hexBinary_unparse_17 = test
  @Test def hexBinary_unparse_18 = test
  @Test def hexBinary_unparse_19 = test

  @Test def hexBinary_variable_unparse_01 = test
  @Test def hexBinary_variable_unparse_02 = test
  @Test def hexBinary_variable_unparse_03 = test
  @Test def hexBinary_variable_unparse_04 = test

  @Test def float_binary_unparse_01 = test
  @Test def double_binary_unparse_01 = test
  @Test def integer_binary_unparse_01 = test
}
