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

object TestSimple extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/simple.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, TDMLImplementation.DaffodilC)
}

class TestSimple extends TdmlTests {
  val tdmlSuite = TestSimple

  @Test def simple_boolean = test
  @Test def simple_byte = test
  @Test def simple_double = test
  @Test def simple_float = test
  @Test def simple_hexBinary = test
  @Test def simple_hexBinaryPrefixed = test
  @Test def simple_int = test
  @Test def simple_integer = test
  @Test def simple_long = test
  @Test def simple_nonNegativeInteger = test
  @Test def simple_short = test
  @Test def simple_unsignedByte = test
  @Test def simple_unsignedInt = test
  @Test def simple_unsignedLong = test
  @Test def simple_unsignedShort = test
}
