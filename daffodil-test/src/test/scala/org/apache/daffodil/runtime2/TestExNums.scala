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

object TestExNums {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner1 = Runner(testDir, "ex_nums.tdml", TDMLImplementation.Daffodil)
  val runner2 = Runner(testDir, "ex_nums.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = {
    runner1.reset
    runner2.reset
  }
}

class TestExNums {
  import TestExNums._

  @Test def runtime1_ex_nums(): Unit = { runner1.runOneTest("ex_nums") }
  @Test def runtime1_error_limited(): Unit = { runner1.runOneTest("runtime1_error_limited") }
  @Test def runtime1_error_on(): Unit = { runner1.runOneTest("runtime1_error_on") }
  @Test def runtime1_error_unparse(): Unit = { runner1.runOneTest("runtime1_error_unparse") }

  @Test def runtime2_ex_nums(): Unit = { runner2.runOneTest("ex_nums") }
  @Test def runtime2_error_parse(): Unit = { runner2.runOneTest("runtime2_error_parse") }
  @Test def runtime2_error_unparse(): Unit = { runner2.runOneTest("runtime2_error_unparse") }
}
