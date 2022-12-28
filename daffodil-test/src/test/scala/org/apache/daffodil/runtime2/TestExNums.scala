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
  val runnerS: Runner = Runner(testDir, "ex_nums.tdml", TDMLImplementation.Daffodil)
  val runnerC: Runner = Runner(testDir, "ex_nums.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = {
    runnerS.reset()
    runnerC.reset()
  }
}

class TestExNums {
  import TestExNums._

  @Test def s_ex_nums(): Unit               = { runnerS.runOneTest("ex_nums") }
  @Test def s_parse_error_off(): Unit       = { runnerS.runOneTest("parse_error_off") }
  @Test def s_parse_error_limited(): Unit   = { runnerS.runOneTest("parse_error_limited") }
  @Test def s_parse_error_on(): Unit        = { runnerS.runOneTest("parse_error_on") }
  @Test def s_unparse_error_off(): Unit     = { runnerS.runOneTest("unparse_error_off") }
  @Test def s_unparse_error_limited(): Unit = { runnerS.runOneTest("unparse_error_limited") }
  @Test def s_unparse_error_on(): Unit      = { runnerS.runOneTest("unparse_error_on") }

  @Test def c_ex_nums(): Unit               = { runnerC.runOneTest("ex_nums") }
  @Test def c_parse_error_off(): Unit       = { runnerC.runOneTest("parse_error_off") }
  @Test def c_parse_error_limited(): Unit   = { runnerC.runOneTest("parse_error_limitedC") }
  @Test def c_parse_error_on(): Unit        = { runnerC.runOneTest("parse_error_on") }
  @Test def c_unparse_error_off(): Unit     = { runnerC.runOneTest("unparse_error_offC") }
  @Test def c_unparse_error_limited(): Unit = { runnerC.runOneTest("unparse_error_limitedC") }
  @Test def c_unparse_error_on(): Unit      = { runnerC.runOneTest("unparse_error_onC") }
}
