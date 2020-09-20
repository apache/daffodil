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

object TestIsrmOrangeToGreen60002 {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner = Runner(testDir, "ISRM_orange_to_green_60002.tdml")

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestIsrmOrangeToGreen60002 {
  import TestIsrmOrangeToGreen60002._

  @Test def test_ISRM_orange_to_green_60002_parse(): Unit = { runner.runOneTest("ISRM_orange_to_green_60002_parse") }
  @Test def test_ISRM_orange_to_green_60002_unparse(): Unit = { runner.runOneTest("ISRM_orange_to_green_60002_unparse") }
}
