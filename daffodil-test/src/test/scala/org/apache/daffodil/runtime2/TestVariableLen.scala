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

object TestVariableLen {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runnerS: Runner = Runner(testDir, "variablelen.tdml", TDMLImplementation.Daffodil)
  val runnerC: Runner = Runner(testDir, "variablelen.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = {
    runnerS.reset()
    runnerC.reset()
  }
}

class TestVariableLen {
  import TestVariableLen._

  @Test def s_fixed(): Unit         = { runnerS.runOneTest("fixed") }
  @Test def s_implicit(): Unit      = { runnerS.runOneTest("implicit") }
  @Test def s_parsed(): Unit        = { runnerS.runOneTest("parsed") }
  @Test def s_expression(): Unit    = { runnerS.runOneTest("expression") }
  @Test def s_expression_00(): Unit = { runnerS.runOneTest("expression_00") }
  @Test def s_expression_01(): Unit = { runnerS.runOneTest("expression_01") }
  @Test def s_expression_16(): Unit = { runnerS.runOneTest("expression_16") }
  @Test def s_expression_17(): Unit = { runnerS.runOneTest("expression_17") }
  @Test def s_stopValue(): Unit     = { runnerS.runOneTest("stopValue") }

  @Test def c_fixed(): Unit         = { runnerC.runOneTest("fixed") }
  @Test def c_expression(): Unit    = { runnerC.runOneTest("expression") }
  @Test def c_expression_00(): Unit = { runnerC.runOneTest("expression_00") }
  @Test def c_expression_01(): Unit = { runnerC.runOneTest("expression_01") }
  @Test def c_expression_16(): Unit = { runnerC.runOneTest("expression_16") }
  @Test def c_expression_17(): Unit = { runnerC.runOneTest("expression_17C") }
  @Test def c_stopValue(): Unit     = { runnerC.runOneTest("stopValue") }
}
