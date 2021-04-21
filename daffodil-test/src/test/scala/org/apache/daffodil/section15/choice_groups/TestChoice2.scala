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

package org.apache.daffodil.section15.choice_groups

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestChoice2 {
  val testDir = "/org/apache/daffodil/section15/choice_groups/"

  val runner = Runner(testDir, "choice1765.tdml")
  val runner1773 = Runner(testDir, "choice1773.tdml")
  val runner2162 = Runner(testDir, "choice2162.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner1773.reset
    runner2162.reset
  }
}

class TestChoice2 {

  import TestChoice2._

  // DFDL-1765
  @Test def test_backtrack1(): Unit = { runner.runOneTest("backtrack1") }
  @Test def test_backtrack2(): Unit = { runner.runOneTest("backtrack2") }
  @Test def test_backtrack3(): Unit = { runner.runOneTest("backtrack2") }
  @Test def test_backtrack4(): Unit = { runner.runOneTest("backtrack4") }

  // DFDL-1773
  @Test def test_choiceSlotAmbiguous1(): Unit = { runner1773.runOneTest("choiceSlotAmbiguous1") }
  @Test def test_choiceSlotAmbiguous2(): Unit = { runner1773.runOneTest("choiceSlotAmbiguous2") }

  // DAFFODIL-1773
  @Test def test_queryStyle1(): Unit = { runner1773.runOneTest("queryStyle1") }
  @Test def test_queryStyle2(): Unit = { runner1773.runOneTest("queryStyle2") }

  // DAFFODIL-2162
  @Test def test_choiceArrayDirectDispatch1(): Unit = { runner2162.runOneTest("choiceArrayDirectDispatch1") }

}
