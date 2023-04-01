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
}
