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
package org.apache.daffodil.runtime1.layers

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestByteSwap {
  private val testDir = "/org/apache/daffodil/layers/"

  private lazy val runner = Runner(testDir, "TestTwoByteSwap.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset()
  }
}

class TestByteSwap {
  import TestByteSwap._

  @Test def test_twobyteswap_01(): Unit = { runner.runOneTest("test_twobyteswap_01") }

  @Test def test_twobyteswap_02(): Unit = { runner.runOneTest("test_twobyteswap_02") }

  @Test def test_twobyteswap_03(): Unit = { runner.runOneTest("test_twobyteswap_03") }

  @Test def test_twobyteswap_04(): Unit = { runner.runOneTest("test_twobyteswap_04") }

  @Test def test_twobyteswap_05(): Unit = { runner.runOneTest("test_twobyteswap_05") }

  @Test def test_twobyteswap_bad_01(): Unit = { runner.runOneTest("test_twobyteswap_bad_01") }

  @Test def test_twobyteswap_bad_02(): Unit = { runner.runOneTest("test_twobyteswap_bad_02") }

  @Test def test_twobyteswap_bad_03(): Unit = { runner.runOneTest("test_twobyteswap_bad_03") }

  @Test def test_twobyteswap_unparse_odd(): Unit = {
    runner.runOneTest("test_twobyteswap_unparse_odd")
  }

}
