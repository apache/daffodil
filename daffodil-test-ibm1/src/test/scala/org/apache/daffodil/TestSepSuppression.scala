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
import org.junit.Test

object TestSepSuppression {
  val testDir = "/test-suite/tresys-contributed/"
  lazy val runner = Runner(testDir, "sepSuppression.tdml")
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
  @Test def test_ptg1_2u_ibm() = { runner.runOneTest("ptg1_2u_ibm") }
  @Test def test_ptg1_3u_ibm() = { runner.runOneTest("ptg1_3u_ibm") }
  @Test def test_ptg1_4u_ibm() = { runner.runOneTest("ptg1_4u_ibm") }
  @Test def test_ptg1_5u_ibm() = { runner.runOneTest("ptg1_5u_ibm") }
  @Test def test_ptg1_6u_ibm() = { runner.runOneTest("ptg1_6u_ibm") }

  @Test def test_ptg2_1p_daf() = { runner.runOneTest("ptg2_1p_daf") }
  @Test def test_ptg3_1p_daf() = { runner.runOneTest("ptg3_1p_daf") }
  @Test def test_ptg3_1u_ibm() = { runner.runOneTest("ptg3_1u_ibm") }

}
