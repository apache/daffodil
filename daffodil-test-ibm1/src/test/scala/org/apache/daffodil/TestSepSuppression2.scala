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

package org.apache.daffodil

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestSepSuppression2 extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/sepSuppression2.tdml"
}

class TestSepSuppression2 extends TdmlTests {
  val tdmlSuite = TestSepSuppression2

  @Test def ptLax0_1u = test
  @Test def ptLax0_2u = test
  @Test def ptLax0_3u = test

  @Test def ptLax1rt = test

  @Test def ptLax2p = test
  @Test def ptLax2u = test
  @Test def ptLax2p2 = test

  @Test def ptLax3rt = test

  @Test def testAnyEmptyTrailing1 = test
}
