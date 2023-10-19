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

import org.apache.daffodil.lib.api.TDMLImplementation
import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestSimpleErrors {
  val testDir = "/org/apache/daffodil/codegen/c/"
  val runner = Runner(testDir, "simple_errors.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = { runner.reset() }
}

class TestSimpleErrors {
  import TestSimpleErrors._

  @Test def simple_boolean_42(): Unit = { runner.runOneTest("simple-boolean-42") }
  @Test def simple_byte_2b(): Unit = { runner.runOneTest("simple-byte-2b") }
  @Test def simple_double_4b(): Unit = { runner.runOneTest("simple-double-4b") }
  @Test def simple_float_NaN(): Unit = { runner.runOneTest("simple-float-NaN") }
  @Test def simple_hexBinary_5b(): Unit = { runner.runOneTest("simple-hexBinary-5b") }
  @Test def simple_hexBinaryPrefixed_4b(): Unit = {
    runner.runOneTest("simple-hexBinaryPrefixed-4b")
  }
  @Test def simple_int_1b(): Unit = { runner.runOneTest("simple-int-1b") }
  @Test def simple_unsignedShort_4b(): Unit = { runner.runOneTest("simple-unsignedShort-4b") }

  @Test def enum_byte_0(): Unit = { runner.runOneTest("enum-byte-0") }
  @Test def enum_double_0_0(): Unit = { runner.runOneTest("enum-double-0.0") }
  @Test def enum_float_0_0(): Unit = { runner.runOneTest("enum-float-0.0") }
  @Test def enum_hexBinary_00000000(): Unit = { runner.runOneTest("enum-hexBinary-00000000") }
  @Test def enum_hexBinaryPrefixed_00000000(): Unit = {
    runner.runOneTest("enum-hexBinaryPrefixed-00000000")
  }
  @Test def enum_int_0(): Unit = { runner.runOneTest("enum-int-0") }
  @Test def enum_integer_0(): Unit = { runner.runOneTest("enum-integer-0") }
  @Test def enum_long_0(): Unit = { runner.runOneTest("enum-long-0") }
  @Test def enum_nonNegativeInteger_0(): Unit = {
    runner.runOneTest("enum-nonNegativeInteger-0")
  }
  @Test def enum_short_0(): Unit = { runner.runOneTest("enum-short-0") }
  @Test def enum_unsignedByte_0(): Unit = { runner.runOneTest("enum-unsignedByte-0") }
  @Test def enum_unsignedInt_0(): Unit = { runner.runOneTest("enum-unsignedInt-0") }
  @Test def enum_unsignedLong_0(): Unit = { runner.runOneTest("enum-unsignedLong-0") }
  @Test def enum_unsignedShort_0(): Unit = { runner.runOneTest("enum-unsignedShort-0") }

  @Test def range_byte_0(): Unit = { runner.runOneTest("range-byte-0") }
  @Test def range_double_0_0(): Unit = { runner.runOneTest("range-double-0.0") }
  @Test def range_float_0_0(): Unit = { runner.runOneTest("range-float-0.0") }
  @Test def range_int_0(): Unit = { runner.runOneTest("range-int-0") }
  @Test def range_integer_0(): Unit = { runner.runOneTest("range-integer-0") }
  @Test def range_long_0(): Unit = { runner.runOneTest("range-long-0") }
  @Test def range_nonNegativeInteger_0(): Unit = {
    runner.runOneTest("range-nonNegativeInteger-0")
  }
  @Test def range_short_0(): Unit = { runner.runOneTest("range-short-0") }
  @Test def range_unsignedByte_0(): Unit = { runner.runOneTest("range-unsignedByte-0") }
  @Test def range_unsignedInt_0(): Unit = { runner.runOneTest("range-unsignedInt-0") }
  @Test def range_unsignedLong_0(): Unit = { runner.runOneTest("range-unsignedLong-0") }
  @Test def range_unsignedShort_0(): Unit = { runner.runOneTest("range-unsignedShort-0") }
}
