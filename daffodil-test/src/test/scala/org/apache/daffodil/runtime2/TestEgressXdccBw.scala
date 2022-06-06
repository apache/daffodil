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

package org.apache.daffodil.runtime2

import org.apache.daffodil.api.TDMLImplementation
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestEgressXdccBw {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner = Runner(testDir, "egress_xdcc_bw.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestEgressXdccBw {
  import TestEgressXdccBw._

  @Test def test_egress_xdcc_bw_11(): Unit = { runner.runOneTest("egress_xdcc_bw_11") }
  @Test def test_egress_xdcc_bw_12(): Unit = { runner.runOneTest("egress_xdcc_bw_12") }
  @Test def test_egress_xdcc_bw_13(): Unit = { runner.runOneTest("egress_xdcc_bw_13") }
  @Test def test_egress_xdcc_bw_14(): Unit = { runner.runOneTest("egress_xdcc_bw_14") }
  @Test def test_egress_xdcc_bw_15(): Unit = { runner.runOneTest("egress_xdcc_bw_15") }
  @Test def test_egress_xdcc_bw_16(): Unit = { runner.runOneTest("egress_xdcc_bw_16") }
  @Test def test_egress_xdcc_bw_17(): Unit = { runner.runOneTest("egress_xdcc_bw_17") }
  @Test def test_egress_xdcc_bw_18(): Unit = { runner.runOneTest("egress_xdcc_bw_18") }
  @Test def test_egress_xdcc_bw_19(): Unit = { runner.runOneTest("egress_xdcc_bw_19") }
  @Test def test_egress_xdcc_bw_20(): Unit = { runner.runOneTest("egress_xdcc_bw_20") }
}
