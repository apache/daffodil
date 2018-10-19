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

object TestLengthKindDelimitedDebug {
  private val testDir = "/org/apache/daffodil/section12/lengthKind/"

  val runner = Runner(testDir, "DelimitedTests.tdml")
  val runnerAB = Runner(testDir, "AB.tdml")
  val runnerAN = Runner(testDir, "AN.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerAB.reset
    runnerAN.reset
  }

}

class TestLengthKindDelimitedDebug {

  import TestLengthKindDelimitedDebug._

  // DAFFODIL-230 dfdl:documentFinalTerminatorCanBeMissing
  @Test def test_NumSeq_10() { runner.runOneTest("NumSeq_10") }

  // DAFFODIL-1975
  @Test def test_NumSeq_13a() { runner.runOneTest("NumSeq_13a") }
}
