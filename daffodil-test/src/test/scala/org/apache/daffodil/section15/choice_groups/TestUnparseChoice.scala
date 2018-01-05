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
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestUnparseChoice {
  val testDir = "/org/apache/daffodil/section15/choice_groups/"
  val runnerCH = Runner(testDir, "choice-unparse.tdml")

  @AfterClass def tearDown() {
    runnerCH.reset
  }
}

class TestUnparseChoice {
  import TestUnparseChoice._

  @Test def test_choice1() { runnerCH.runOneTest("choice1") }
  @Test def test_choice2() { runnerCH.runOneTest("choice2") }
  @Test def test_choice3() { runnerCH.runOneTest("choice3") }
  @Test def test_choice4() { runnerCH.runOneTest("choice4") }
  @Test def test_choice5() { runnerCH.runOneTest("choice5") }
  @Test def test_choice6() { runnerCH.runOneTest("choice6") }
}
