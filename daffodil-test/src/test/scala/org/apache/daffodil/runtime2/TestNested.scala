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

package org.apache.daffodil.runtime2

import org.apache.daffodil.api.TDMLImplementation
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestNested {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner: Runner = Runner(testDir, "nested.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestNested {
  import TestNested._

  @Test def test_nested_struct(): Unit = { runner.runOneTest("nested_struct") }
  @Test def test_nested_union_bar(): Unit = { runner.runOneTest("nested_union_bar") }
  @Test def test_nested_union_foo(): Unit = { runner.runOneTest("nested_union_foo") }
}
