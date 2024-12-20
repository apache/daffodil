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

package org.apache.daffodil.section05.simple_types

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestEncodings extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/Encodings.tdml"
}

class TestEncodings extends TdmlTests {
  val tdmlSuite = TestEncodings

  @Test def f293u003_01 = test
  @Test def f293u003_02 = test
  @Test def f422u001_01 = test
  @Test def f422u001_02 = test
  @Test def f422u001_03 = test
  @Test def f746u002_01 = test
  @Test def f746u002_02 = test
  @Test def f746u002_03 = test
  @Test def f747u001_01 = test
  @Test def f747u001_02 = test
  @Test def f747u001_03 = test
  @Test def f769u002_01 = test
  @Test def f769u002_02 = test
  @Test def f769u002_03 = test
  @Test def f336u002_01 = test
  @Test def f336u002_02 = test
  @Test def f336u002_03 = test
}
