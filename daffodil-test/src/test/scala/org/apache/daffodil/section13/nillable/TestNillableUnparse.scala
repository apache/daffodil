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

package org.apache.daffodil.section13.nillable

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit._

object TestNillableUnparse {
  val testDir = "/org/apache/daffodil/section13/nillable/"
  val runnerLN = Runner(testDir, "literal-value-nils-unparse.tdml")
  val runnerLC = Runner(testDir, "literal-character-nils-unparse.tdml")

  @AfterClass def tearDown(): Unit = {
    runnerLN.reset
    runnerLC.reset
  }
}

class TestNillableUnparse {

  import TestNillableUnparse._

  @Test def test_scalar_nonDefaultable_nillable(): Unit = { runnerLN.runOneTest("scalar_nonDefaultable_nillable") }
  @Test def test_scalar_nonDefaultable_nillable_02(): Unit = { runnerLN.runOneTest("scalar_nonDefaultable_nillable_02") }
  @Test def test_scalar_nonDefaultable_nillable_03(): Unit = { runnerLN.runOneTest("scalar_nonDefaultable_nillable_03") }

  @Test def test_text_complex_nil(): Unit = { runnerLN.runOneTest("text_complex_nil") }
  @Test def test_text_complex_nil2(): Unit = { runnerLN.runOneTest("text_complex_nil2") }
  @Test def test_text_complex_nil3(): Unit = { runnerLN.runOneTest("text_complex_nil3") }
  @Test def test_text_complex_nil4(): Unit = { runnerLN.runOneTest("text_complex_nil4") }

  @Test def test_text_nil_only1(): Unit = { runnerLN.runOneTest("text_nil_only1") }
  @Test def test_text_nil_only2(): Unit = { runnerLN.runOneTest("text_nil_only2") }
  @Test def test_text_nil_only3(): Unit = { runnerLN.runOneTest("text_nil_only3") }
  @Test def test_text_nil_only4(): Unit = { runnerLN.runOneTest("text_nil_only4") }
  @Test def test_text_nil_only5(): Unit = { runnerLN.runOneTest("text_nil_only5") }
  @Test def test_text_nil_only6(): Unit = { runnerLN.runOneTest("text_nil_only6") }
  @Test def test_text_nil_only7(): Unit = { runnerLN.runOneTest("text_nil_only7") }
  @Test def test_text_nil_only8(): Unit = { runnerLN.runOneTest("text_nil_only8") }
  @Test def test_text_nil_only9(): Unit = { runnerLN.runOneTest("text_nil_only9") }
  @Test def test_text_nil_only10(): Unit = { runnerLN.runOneTest("text_nil_only10") }
  @Test def test_text_nil_only11(): Unit = { runnerLN.runOneTest("text_nil_only11") }
  @Test def test_text_nil_only12(): Unit = { runnerLN.runOneTest("text_nil_only12") }
  @Test def test_text_nil_only13(): Unit = { runnerLN.runOneTest("text_nil_only13") }
  @Test def test_text_nil_only14(): Unit = { runnerLN.runOneTest("text_nil_only14") }
  @Test def test_text_nil_only15(): Unit = { runnerLN.runOneTest("text_nil_only15") }
  @Test def test_text_nil_only16(): Unit = { runnerLN.runOneTest("text_nil_only16") }
  @Test def test_text_nil_only17(): Unit = { runnerLN.runOneTest("text_nil_only17") }

  @Test def test_text_nil_characterClass_01(): Unit = { runnerLN.runOneTest("text_nil_characterClass_01") }
  @Test def test_text_nil_characterClass_02(): Unit = { runnerLN.runOneTest("text_nil_characterClass_02") }
  @Test def test_text_nil_characterClass_03(): Unit = { runnerLN.runOneTest("text_nil_characterClass_03") }
  @Test def test_text_nil_characterClass_04(): Unit = { runnerLN.runOneTest("text_nil_characterClass_04") }
  @Test def test_text_nil_characterClass_05(): Unit = { runnerLN.runOneTest("text_nil_characterClass_05") }
  @Test def test_text_nil_characterClass_06(): Unit = { runnerLN.runOneTest("text_nil_characterClass_06") }

  @Test def test_text_lit_char_01(): Unit = { runnerLC.runOneTest("text_01") }
  @Test def test_text_lit_char_01a(): Unit = { runnerLC.runOneTest("text_01a") }

}
