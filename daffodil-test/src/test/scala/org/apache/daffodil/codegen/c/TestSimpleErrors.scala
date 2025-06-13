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

package org.apache.daffodil.codegen.c

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestSimpleErrors extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/simple_errors.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, TDMLImplementation.DaffodilC)
}

class TestSimpleErrors extends TdmlTests {
  val tdmlSuite = TestSimpleErrors

  @Test def simple_boolean_42 = test
  @Test def simple_byte_2b = test
  @Test def simple_double_4b = test
  @Test def simple_float_NaN = test
  @Test def simple_hexBinary_5b = test
  @Test def simple_hexBinaryPrefixed_4b = test
  @Test def simple_int_1b = test
  @Test def simple_unsignedShort_4b = test

  @Test def enum_byte_0 = test
  @Test def enum_double_0_0 = test
  @Test def enum_float_0_0 = test
  @Test def enum_hexBinary_00000000 = test
  @Test def enum_hexBinaryPrefixed_00000000 = test
  @Test def enum_int_0 = test
  @Test def enum_integer_0 = test
  @Test def enum_long_0 = test
  @Test def enum_nonNegativeInteger_0 = test
  @Test def enum_short_0 = test
  @Test def enum_unsignedByte_0 = test
  @Test def enum_unsignedInt_0 = test
  @Test def enum_unsignedLong_0 = test
  @Test def enum_unsignedShort_0 = test

  @Test def range_byte_0 = test
  @Test def range_double_0_0 = test
  @Test def range_float_0_0 = test
  @Test def range_int_0 = test
  @Test def range_integer_0 = test
  @Test def range_long_0 = test
  @Test def range_nonNegativeInteger_0 = test
  @Test def range_short_0 = test
  @Test def range_unsignedByte_0 = test
  @Test def range_unsignedInt_0 = test
  @Test def range_unsignedLong_0 = test
  @Test def range_unsignedShort_0 = test
}
