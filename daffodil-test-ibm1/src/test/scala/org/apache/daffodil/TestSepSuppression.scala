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

package org.apache.daffodil

import org.apache.daffodil.tdml.Runner
import org.junit.{ AfterClass, Test }

object TestSepSuppression {
  val testDir = "/test-suite/tresys-contributed/"
  val runner = Runner(testDir, "sepSuppression.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestSepSuppression {
  import TestSepSuppression._

  @Test def test_ptg1_1p() = { runner.runOneTest("ptg1_1p") }
  @Test def test_ptg1_2p() = { runner.runOneTest("ptg1_2p") }
  @Test def test_ptg1_3p() = { runner.runOneTest("ptg1_3p") }
  @Test def test_ptg1_4p() = { runner.runOneTest("ptg1_4p") }
  @Test def test_ptg1_5p() = { runner.runOneTest("ptg1_5p") }
  @Test def test_ptg1_6p() = { runner.runOneTest("ptg1_6p") }

  @Test def test_ptg1_1u() = { runner.runOneTest("ptg1_1u") }
  @Test def test_ptg1_2u() = { runner.runOneTest("ptg1_2u") }
  @Test def test_ptg1_3u() = { runner.runOneTest("ptg1_3u") }
  @Test def test_ptg1_4u() = { runner.runOneTest("ptg1_4u") }
  @Test def test_ptg1_5u() = { runner.runOneTest("ptg1_5u") }
  @Test def test_ptg1_6u() = { runner.runOneTest("ptg1_6u") }

  @Test def test_ptg2_1p() = { runner.runOneTest("ptg2_1p") }
  @Test def test_ptg2_1u() = { runner.runOneTest("ptg2_1u") }

  @Test def test_ptg3_1p() = { runner.runOneTest("ptg3_1p") }
  @Test def test_ptg3_1u() = { runner.runOneTest("ptg3_1u") }
  @Test def test_ptg3_2p_daf() = { runner.runOneTest("ptg3_2p_daf") }

}
