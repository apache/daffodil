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

object TestSepSuppression2 {
  val testDir = "/test-suite/tresys-contributed/"
  val runner2 = Runner(testDir, "sepSuppression2.tdml")

  @AfterClass def shutDown(): Unit = {
    runner2.reset
  }
}

class TestSepSuppression2 {
  import TestSepSuppression2._

  @Test def test_ptLax0_1u() = { runner2.runOneTest("ptLax0_1u") }
  @Test def test_ptLax0_2u() = { runner2.runOneTest("ptLax0_2u") }
  @Test def test_ptLax0_3u() = { runner2.runOneTest("ptLax0_3u") }

  @Test def test_ptLax1rt() = { runner2.runOneTest("ptLax1rt") }

  @Test def test_ptLax2p() = { runner2.runOneTest("ptLax2p") }
  @Test def test_ptLax2u() = { runner2.runOneTest("ptLax2u") }
  @Test def test_ptLax2p2() = { runner2.runOneTest("ptLax2p2") }

  @Test def test_ptLax3rt() = { runner2.runOneTest("ptLax3rt") }

  @Test def test_testAnyEmptyTrailing1() = { runner2.runOneTest("testAnyEmptyTrailing1") }
}
