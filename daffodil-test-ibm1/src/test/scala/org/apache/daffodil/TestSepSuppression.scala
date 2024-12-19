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

object TestSepSuppression extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/sepSuppression.tdml"
}

class TestSepSuppression extends TdmlTests {
  val tdmlSuite = TestSepSuppression

  @Test def ptg1_1p = test
  @Test def ptg1_2p = test
  @Test def ptg1_3p = test
  @Test def ptg1_4p = test
  @Test def ptg1_5p = test
  @Test def ptg1_6p = test

  @Test def ptg1_1u = test
  @Test def ptg1_2u = test
  @Test def ptg1_3u = test
  @Test def ptg1_4u = test
  @Test def ptg1_5u = test
  @Test def ptg1_6u = test

  @Test def ptg2_1p = test
  @Test def ptg2_1u = test

  @Test def ptg3_1p = test
  @Test def ptg3_1u = test
  @Test def ptg3_2p_daf = test
}
