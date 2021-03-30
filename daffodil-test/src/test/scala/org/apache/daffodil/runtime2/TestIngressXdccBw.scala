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

object TestIngressXdccBw {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner = Runner(testDir, "ingress_xdcc_bw.tdml")

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestIngressXdccBw {
  import TestIngressXdccBw._

  @Test def test_ingress_xdcc_bw_parse_111(): Unit = { runner.runOneTest("ingress_xdcc_bw_parse_111") }
  @Test def test_ingress_xdcc_bw_unparse_111(): Unit = { runner.runOneTest("ingress_xdcc_bw_unparse_111") }
  @Test def test_ingress_xdcc_bw_parse_112(): Unit = { runner.runOneTest("ingress_xdcc_bw_parse_112") }
  @Test def test_ingress_xdcc_bw_unparse_112(): Unit = { runner.runOneTest("ingress_xdcc_bw_unparse_112") }
  @Test def test_ingress_xdcc_bw_parse_113(): Unit = { runner.runOneTest("ingress_xdcc_bw_parse_113") }
  @Test def test_ingress_xdcc_bw_unparse_113(): Unit = { runner.runOneTest("ingress_xdcc_bw_unparse_113") }
  @Test def test_ingress_xdcc_bw_parse_114(): Unit = { runner.runOneTest("ingress_xdcc_bw_parse_114") }
  @Test def test_ingress_xdcc_bw_unparse_114(): Unit = { runner.runOneTest("ingress_xdcc_bw_unparse_114") }
  @Test def test_ingress_xdcc_bw_parse_115(): Unit = { runner.runOneTest("ingress_xdcc_bw_parse_115") }
  @Test def test_ingress_xdcc_bw_unparse_115(): Unit = { runner.runOneTest("ingress_xdcc_bw_unparse_115") }
  @Test def test_ingress_xdcc_bw_parse_116(): Unit = { runner.runOneTest("ingress_xdcc_bw_parse_116") }
  @Test def test_ingress_xdcc_bw_unparse_116(): Unit = { runner.runOneTest("ingress_xdcc_bw_unparse_116") }
}
