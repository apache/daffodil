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

package org.apache.daffodil.section13.zoned

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestPV extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/zoned/pv.tdml"
}

class TestPV extends TdmlTests {
  val tdmlSuite = TestPV

  @Test def vpattern_01 = test
  @Test def vpattern_02 = test
  @Test def vpattern_03 = test
  @Test def vpattern_04 = test
  @Test def vpattern_05 = test
  @Test def vpattern_06 = test
  @Test def vpattern_07 = test
  @Test def vpattern_08 = test
  @Test def vpattern_09 = test

  @Test def vpattern_zero = test
  @Test def vpattern_ZZZ = test

  @Test def vpattern_float = test
  @Test def vpattern_double = test
  @Test def vpattern_float_NaN = test
  @Test def vpattern_double_NaN = test
  @Test def vpattern_float_Inf = test
  @Test def vpattern_double_Inf = test

  @Test def float_vpattern_01 = test
  @Test def double_vpattern_01 = test

  @Test def vpattern_bad_01 = test
  @Test def vpattern_bad_02 = test
  @Test def vpattern_bad_03 = test
  @Test def vpattern_warn_04 = test

  @Test def zoned_vpattern_01 = test
  @Test def zoned_vpattern_02 = test
  @Test def zoned_vpattern_03 = test
  @Test def zoned_vpattern_04 = test
  @Test def zoned_vpattern_05 = test

  @Test def bad_byte_vpattern_01 = test

  @Test def zoned_float_vpattern_01 = test
  @Test def zoned_double_vpattern_01 = test

  @Test def zoned_vpattern_bad_01 = test
  @Test def zoned_vpattern_bad_02 = test
  @Test def zoned_vpattern_bad_03 = test

  @Test def ppattern_01 = test
  @Test def ppattern_02 = test
}
