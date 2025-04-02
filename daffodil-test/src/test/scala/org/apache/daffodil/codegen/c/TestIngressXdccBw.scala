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

package org.apache.daffodil.codegen.c

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestIngressXdccBw extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/ingress_xdcc_bw.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, TDMLImplementation.DaffodilC)
}

class TestIngressXdccBw extends TdmlTests {
  val tdmlSuite = TestIngressXdccBw

  @Test def ingress_xdcc_bw_111 = test
  @Test def ingress_xdcc_bw_112 = test
  @Test def ingress_xdcc_bw_113 = test
  @Test def ingress_xdcc_bw_114 = test
  @Test def ingress_xdcc_bw_115 = test
  @Test def ingress_xdcc_bw_116 = test
}
