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

import org.apache.daffodil.lib.api.TDMLImplementation
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestPadTest {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runnerS: Runner = Runner(testDir, "padtest.tdml", TDMLImplementation.Daffodil)
  val runnerC: Runner = Runner(testDir, "padtest.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = {
    runnerS.reset()
    runnerC.reset()
  }
}

class TestPadTest {
  import TestPadTest._

  @Test def s_padtest_00(): Unit = { runnerS.runOneTest("padtest_00") }
  @Test def s_padtest_01(): Unit = { runnerS.runOneTest("padtest_01") }
  @Test def s_padtest_16(): Unit = { runnerS.runOneTest("padtest_16") }
  @Test def s_padtest_17(): Unit = { runnerS.runOneTest("padtest_17") }

  @Test def c_padtest_00(): Unit = { runnerC.runOneTest("padtest_00") }
  @Test def c_padtest_01(): Unit = { runnerC.runOneTest("padtest_01") }
  @Test def c_padtest_16(): Unit = { runnerC.runOneTest("padtest_16") }
  @Test def c_padtest_17(): Unit = { runnerC.runOneTest("padtest_17") }
}
