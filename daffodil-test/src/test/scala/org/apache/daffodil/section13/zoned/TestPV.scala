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

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestPV {
  val testDir = "/org/apache/daffodil/section13/zoned/"
  val runner = Runner(testDir, "pv.tdml")

  @AfterClass def shutdown(): Unit = {
    runner.reset
  }

}

class TestPV {
  import TestPV._

  @Test def vpattern_01(): Unit = { runner.runOneTest("vpattern_01") }
  @Test def vpattern_02(): Unit = { runner.runOneTest("vpattern_02") }
  @Test def vpattern_03(): Unit = { runner.runOneTest("vpattern_03") }
  @Test def vpattern_04(): Unit = { runner.runOneTest("vpattern_04") }
  @Test def vpattern_05(): Unit = { runner.runOneTest("vpattern_05") }
  @Test def vpattern_06(): Unit = { runner.runOneTest("vpattern_06") }
  @Test def vpattern_07(): Unit = { runner.runOneTest("vpattern_07") }

  @Test def vpattern_zero(): Unit = { runner.runOneTest("vpattern_zero") }
  @Test def vpattern_ZZZ(): Unit = { runner.runOneTest("vpattern_ZZZ") }

  @Test def vpattern_float(): Unit = { runner.runOneTest("vpattern_float") }
  @Test def vpattern_double(): Unit = { runner.runOneTest("vpattern_double") }
  @Test def vpattern_float_NaN(): Unit = { runner.runOneTest("vpattern_float_NaN") }
  @Test def vpattern_double_NaN(): Unit = { runner.runOneTest("vpattern_double_NaN") }
  @Test def vpattern_float_Inf(): Unit = { runner.runOneTest("vpattern_float_Inf") }
  @Test def vpattern_double_Inf(): Unit = { runner.runOneTest("vpattern_double_Inf") }

  @Test def float_vpattern_01(): Unit = { runner.runOneTest("float_vpattern_01") }
  @Test def double_vpattern_01(): Unit = { runner.runOneTest("double_vpattern_01") }

  @Test def vpattern_bad_01(): Unit = { runner.runOneTest("vpattern_bad_01") }
  @Test def vpattern_bad_02(): Unit = { runner.runOneTest("vpattern_bad_02") }
  @Test def vpattern_bad_03(): Unit = { runner.runOneTest("vpattern_bad_03") }
  @Test def vpattern_warn_04(): Unit = { runner.runOneTest("vpattern_warn_04") }


  @Test def zoned_vpattern_01(): Unit = { runner.runOneTest("zoned_vpattern_01") }
  @Test def zoned_vpattern_02(): Unit = { runner.runOneTest("zoned_vpattern_02") }
  @Test def zoned_vpattern_03(): Unit = { runner.runOneTest("zoned_vpattern_03") }
  @Test def zoned_vpattern_04(): Unit = { runner.runOneTest("zoned_vpattern_04") }
  @Test def zoned_vpattern_05(): Unit = { runner.runOneTest("zoned_vpattern_05") }

  @Test def bad_byte_vpattern_01(): Unit = { runner.runOneTest("bad_byte_vpattern_01") }

  @Test def zoned_float_vpattern_01(): Unit = { runner.runOneTest("zoned_float_vpattern_01") }
  @Test def zoned_double_vpattern_01(): Unit = { runner.runOneTest("zoned_double_vpattern_01") }

  @Test def zoned_vpattern_bad_01(): Unit = { runner.runOneTest("zoned_vpattern_bad_01") }
  @Test def zoned_vpattern_bad_02(): Unit = { runner.runOneTest("zoned_vpattern_bad_02") }
  @Test def zoned_vpattern_bad_03(): Unit = { runner.runOneTest("zoned_vpattern_bad_03") }

  @Test def ppattern_01(): Unit = { runner.runOneTest("ppattern_01") }
  @Test def ppattern_02(): Unit = { runner.runOneTest("ppattern_02") }


}


