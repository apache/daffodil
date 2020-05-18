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

  @AfterClass def tearDown() {
    runnerLN.reset
    runnerLC.reset
  }
}

class TestNillableUnparse {

  import TestNillableUnparse._

  @Test def test_scalar_nonDefaultable_nillable() { runnerLN.runOneTest("scalar_nonDefaultable_nillable") }
  @Test def test_scalar_nonDefaultable_nillable_02() { runnerLN.runOneTest("scalar_nonDefaultable_nillable_02") }
  @Test def test_scalar_nonDefaultable_nillable_03() { runnerLN.runOneTest("scalar_nonDefaultable_nillable_03") }

  @Test def test_text_complex_nil() { runnerLN.runOneTest("text_complex_nil") }
  @Test def test_text_complex_nil2() { runnerLN.runOneTest("text_complex_nil2") }
  @Test def test_text_complex_nil3() { runnerLN.runOneTest("text_complex_nil3") }

  @Test def test_text_nil_only1() { runnerLN.runOneTest("text_nil_only1") }
  @Test def test_text_nil_only2() { runnerLN.runOneTest("text_nil_only2") }
  @Test def test_text_nil_only3() { runnerLN.runOneTest("text_nil_only3") }
  @Test def test_text_nil_only4() { runnerLN.runOneTest("text_nil_only4") }
  @Test def test_text_nil_only5() { runnerLN.runOneTest("text_nil_only5") }
  @Test def test_text_nil_only6() { runnerLN.runOneTest("text_nil_only6") }
  @Test def test_text_nil_only7() { runnerLN.runOneTest("text_nil_only7") }
  @Test def test_text_nil_only8() { runnerLN.runOneTest("text_nil_only8") }
  @Test def test_text_nil_only9() { runnerLN.runOneTest("text_nil_only9") }
  @Test def test_text_nil_only10() { runnerLN.runOneTest("text_nil_only10") }
  @Test def test_text_nil_only11() { runnerLN.runOneTest("text_nil_only11") }
  @Test def test_text_nil_only12() { runnerLN.runOneTest("text_nil_only12") }
  @Test def test_text_nil_only13() { runnerLN.runOneTest("text_nil_only13") }
  @Test def test_text_nil_only14() { runnerLN.runOneTest("text_nil_only14") }
  @Test def test_text_nil_only15() { runnerLN.runOneTest("text_nil_only15") }
  @Test def test_text_nil_only16() { runnerLN.runOneTest("text_nil_only16") }
  @Test def test_text_nil_only17() { runnerLN.runOneTest("text_nil_only17") }

  @Test def test_text_nil_characterClass_01() { runnerLN.runOneTest("text_nil_characterClass_01") }
  @Test def test_text_nil_characterClass_02() { runnerLN.runOneTest("text_nil_characterClass_02") }
  @Test def test_text_nil_characterClass_03() { runnerLN.runOneTest("text_nil_characterClass_03") }
  @Test def test_text_nil_characterClass_04() { runnerLN.runOneTest("text_nil_characterClass_04") }
  @Test def test_text_nil_characterClass_05() { runnerLN.runOneTest("text_nil_characterClass_05") }
  @Test def test_text_nil_characterClass_06() { runnerLN.runOneTest("text_nil_characterClass_06") }

  @Test def test_text_lit_char_01() { runnerLC.runOneTest("text_01") }
  @Test def test_text_lit_char_01a() { runnerLC.runOneTest("text_01a") }

}
