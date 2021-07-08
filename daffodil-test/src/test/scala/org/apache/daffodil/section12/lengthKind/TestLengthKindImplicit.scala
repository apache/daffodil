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

package org.apache.daffodil.section12.lengthKind

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLengthKindImplicit {
  val testDir = "/org/apache/daffodil/section12/lengthKind/"
  val runner_01 = Runner(testDir, "implicit.tdml")

  @AfterClass def shutDown: Unit = {
    runner_01.reset
  }

}

class TestLengthKindImplicit {

  import TestLengthKindImplicit._

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // runner.runOneTest("test_name")
  // }

  @Test def test_nested_seq(): Unit = { runner_01.runOneTest("nested_seq") }
  @Test def test_nested_seq_01(): Unit = { runner_01.runOneTest("nested_seq_01") }

  @Test def test_implicit_with_len(): Unit = { runner_01.runOneTest("implicit_with_len") }
  @Test def test_implicit_ignored_len(): Unit = { runner_01.runOneTest("implicit_ignored_len") }
  @Test def test_implicitLenTime(): Unit = { runner_01.runOneTest("implicitLenTime") }

}
