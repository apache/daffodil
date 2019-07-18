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

package org.apache.daffodil.section05.simple_types

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestEncodings {
  private val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "Encodings.tdml")

  @AfterClass def shutdown {
    runner.reset
  }
}

class TestEncodings {
  import TestEncodings._

  @Test def test_f293u003_01() { runner.runOneTest("f293u003_01") }
  @Test def test_f293u003_02() { runner.runOneTest("f293u003_02") }
  @Test def test_f422u001_01() { runner.runOneTest("f422u001_01") }
  @Test def test_f422u001_02() { runner.runOneTest("f422u001_02") }
  @Test def test_f422u001_03() { runner.runOneTest("f422u001_03") }
  @Test def test_f746u002_01() { runner.runOneTest("f746u002_01") }
  @Test def test_f746u002_02() { runner.runOneTest("f746u002_02") }
  @Test def test_f746u002_03() { runner.runOneTest("f746u002_03") }
  @Test def test_f747u001_01() { runner.runOneTest("f747u001_01") }
  @Test def test_f747u001_02() { runner.runOneTest("f747u001_02") }
  @Test def test_f747u001_03() { runner.runOneTest("f747u001_03") }
  @Test def test_f769u002_01() { runner.runOneTest("f769u002_01") }
  @Test def test_f769u002_02() { runner.runOneTest("f769u002_02") }
  @Test def test_f769u002_03() { runner.runOneTest("f769u002_03") }
  @Test def test_f336u002_01() { runner.runOneTest("f336u002_01") }
  @Test def test_f336u002_02() { runner.runOneTest("f336u002_02") }
  @Test def test_f336u002_03() { runner.runOneTest("f336u002_03") }
}
