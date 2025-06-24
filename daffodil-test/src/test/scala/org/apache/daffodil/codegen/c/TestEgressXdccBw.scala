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

package org.apache.daffodil.codegen.c

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestEgressXdccBw extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/egress_xdcc_bw.tdml"
  override def createRunner() = Runner(tdmlDir, tdmlFile, TDMLImplementation.DaffodilC)
}

class TestEgressXdccBw extends TdmlTests {
  val tdmlSuite = TestEgressXdccBw

  @Test def egress_xdcc_bw_11 = test
  @Test def egress_xdcc_bw_12 = test
  @Test def egress_xdcc_bw_13 = test
  @Test def egress_xdcc_bw_14 = test
  @Test def egress_xdcc_bw_15 = test
  @Test def egress_xdcc_bw_16 = test
  @Test def egress_xdcc_bw_17 = test
  @Test def egress_xdcc_bw_18 = test
  @Test def egress_xdcc_bw_19 = test
  @Test def egress_xdcc_bw_20 = test
}
