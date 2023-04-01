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

object TestSimple {
  val testDir = "/org/apache/daffodil/codegen/c/"
  val runner = Runner(testDir, "simple.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = { runner.reset() }
}

class TestSimple {
  import TestSimple._

  @Test def simple_boolean(): Unit = { runner.runOneTest("simple-boolean") }
  @Test def simple_byte(): Unit = { runner.runOneTest("simple-byte") }
  @Test def simple_double(): Unit = { runner.runOneTest("simple-double") }
  @Test def simple_float(): Unit = { runner.runOneTest("simple-float") }
  @Test def simple_hexBinary(): Unit = { runner.runOneTest("simple-hexBinary") }
  @Test def simple_hexBinaryPrefixed(): Unit = { runner.runOneTest("simple-hexBinaryPrefixed") }
  @Test def simple_int(): Unit = { runner.runOneTest("simple-int") }
  @Test def simple_integer(): Unit = { runner.runOneTest("simple-integer") }
  @Test def simple_long(): Unit = { runner.runOneTest("simple-long") }
  @Test def simple_nonNegativeInteger(): Unit = {
    runner.runOneTest("simple-nonNegativeInteger")
  }
  @Test def simple_short(): Unit = { runner.runOneTest("simple-short") }
  @Test def simple_unsignedByte(): Unit = { runner.runOneTest("simple-unsignedByte") }
  @Test def simple_unsignedInt(): Unit = { runner.runOneTest("simple-unsignedInt") }
  @Test def simple_unsignedLong(): Unit = { runner.runOneTest("simple-unsignedLong") }
  @Test def simple_unsignedShort(): Unit = { runner.runOneTest("simple-unsignedShort") }
}
