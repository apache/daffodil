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

package org.apache.daffodil.section00.general

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestParseUnparsePolicy {
  val testDir = "/org/apache/daffodil/section00/general/"
  val runner = Runner(testDir, "parseUnparsePolicy.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}


class TestParseUnparsePolicy {
  import TestParseUnparsePolicy._

  @Test def test_pb_parse(): Unit =   { runner.runOneTest("pb_parse") }
  @Test def test_pb_unparse(): Unit = { runner.runOneTest("pb_unparse") }
  @Test def test_pp_parse(): Unit =   { runner.runOneTest("pp_parse") }
  @Test def test_pp_unparse(): Unit = { runner.runOneTest("pp_unparse") }
  @Test def test_pu(): Unit =         { runner.runOneTest("pu") }

  @Test def test_ub_parse(): Unit =   { runner.runOneTest("ub_parse") }
  @Test def test_ub_unparse(): Unit = { runner.runOneTest("ub_unparse") }
  @Test def test_uu_parse(): Unit =   { runner.runOneTest("uu_parse") }
  @Test def test_uu_unparse(): Unit = { runner.runOneTest("uu_unparse") }
  @Test def test_up(): Unit =         { runner.runOneTest("up") }

  @Test def test_bb(): Unit = { runner.runOneTest("bb") }
  @Test def test_bp(): Unit = { runner.runOneTest("bp") }
  @Test def test_bu(): Unit = { runner.runOneTest("bu") }

}
