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

package org.apache.daffodil.layers

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestIPv4 {

  val testDir = "/org/apache/daffodil/layers/"
  val runner = Runner(testDir, "IPv4.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestIPv4 {

  import TestIPv4._

  @Test def test_IPv4_1(): Unit = { runner.runOneTest("IPv4_1") }

  // DAFFODIL-2608
  @Test def test_IPv4_array(): Unit = { runner.runOneTest("IPv4_array") }

  @Test def test_IPv4_1e(): Unit = { runner.runOneTest("IPv4_1e") }

  @Test def test_IPv4_2(): Unit = { runner.runOneTest("IPv4_2") }

  @Test def test_IPv4_1u(): Unit = { runner.runOneTest("IPv4_1u") }
  @Test def test_IPv4_2u(): Unit = { runner.runOneTest("IPv4_2u") }


}
