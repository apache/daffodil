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

package org.apache.daffodil.section14.occursCountKind

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOCKImplicit {
  val testDir = "/org/apache/daffodil/section14/occursCountKind/"
  val runner = Runner(testDir, "ockImplicit.tdml")

  @AfterClass def shutDown {
    runner.reset
  }

}

class TestOCKImplicit {
  import TestOCKImplicit._

  @Test def test_ockImplicit1() { runner.runOneTest("ockImplicit1") }
  @Test def test_ockImplicit2() { runner.runOneTest("ockImplicit2") }
  @Test def test_ockImplicit3() { runner.runOneTest("ockImplicit3") }
  @Test def test_ockImplicit4() { runner.runOneTest("ockImplicit4") }
  @Test def test_ockImplicit5() { runner.runOneTest("ockImplicit5") }
  @Test def test_ockImplicit6() { runner.runOneTest("ockImplicit6") }
  @Test def test_ockImplicit7() { runner.runOneTest("ockImplicit7") }
  @Test def test_ockImplicit8() { runner.runOneTest("ockImplicit8") }
  @Test def test_ockImplicit9() { runner.runOneTest("ockImplicit9") }
  @Test def test_ockImplicit10() { runner.runOneTest("ockImplicit10") }
  @Test def test_ockImplicit11() { runner.runOneTest("ockImplicit11") }
  @Test def test_ockImplicit12() { runner.runOneTest("ockImplicit12") }
  @Test def test_ockImplicit13() { runner.runOneTest("ockImplicit13") }
  @Test def test_ockImplicit14() { runner.runOneTest("ockImplicit14") }
  @Test def test_ockImplicit15() { runner.runOneTest("ockImplicit15") }

  @Test def test_ockImplicit16() { runner.runOneTest("ockImplicit16") }
  @Test def test_ockImplicit17() { runner.runOneTest("ockImplicit17") }
  @Test def test_ockImplicit18() { runner.runOneTest("ockImplicit18") }
  @Test def test_ockImplicit19() { runner.runOneTest("ockImplicit19") }

  @Test def test_ockImplicit20() { runner.runOneTest("ockImplicit20") }
  @Test def test_ockImplicit21() { runner.runOneTest("ockImplicit21") }
  @Test def test_ockImplicit22() { runner.runOneTest("ockImplicit22") }
  @Test def test_ockImplicit23() { runner.runOneTest("ockImplicit23") }
  //DFDL-1662
  @Test def test_ockImplicit24() { runner.runOneTest("ockImplicit24") }
}
