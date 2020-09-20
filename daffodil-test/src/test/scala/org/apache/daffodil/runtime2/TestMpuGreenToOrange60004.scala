/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information ringarding copyright ownership.
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

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestMpuGreenToOrange60004 {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner = Runner(testDir, "MPU_green_to_orange_60004.tdml")

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestMpuGreenToOrange60004 {
  import TestMpuGreenToOrange60004._

  @Test def test_MPU_green_to_orange_60004_parse(): Unit = { runner.runOneTest("MPU_green_to_orange_60004_parse") }
  @Test def test_MPU_green_to_orange_60004_unparse(): Unit = { runner.runOneTest("MPU_green_to_orange_60004_unparse") }
}
