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

package org.apache.daffodil.section31.escape_characters

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEscapes {
  val testDir = "/org/apache/daffodil/section31/escape_characters/"
  val runner = Runner(testDir, "Escapes.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }
}

class TestEscapes {
  import TestEscapes._

  @Test def test_escape_entry1() { runner.runOneTest("escape_entry1") }
  @Test def test_escape_entry2() { runner.runOneTest("escape_entry2") }
  @Test def test_escape_entry3() { runner.runOneTest("escape_entry3") }
  @Test def test_escape_entry4() { runner.runOneTest("escape_entry4") }
  @Test def test_escape_entry5() { runner.runOneTest("escape_entry5") }
  @Test def test_escape_entry6() { runner.runOneTest("escape_entry6") }
  @Test def test_escape_entry7() { runner.runOneTest("escape_entry7") }
  @Test def test_escape_entry8() { runner.runOneTest("escape_entry8") }
  @Test def test_escape_entry9() { runner.runOneTest("escape_entry9") }
  @Test def test_escape_entry10() { runner.runOneTest("escape_entry10") }
  @Test def test_escape_entry11() { runner.runOneTest("escape_entry11") }
  @Test def test_escape_entry12() { runner.runOneTest("escape_entry12") }

  @Test def test_escape_entry2_1() { runner.runOneTest("escape_entry2_1") }
  @Test def test_escape_entry2_2() { runner.runOneTest("escape_entry2_2") }
  @Test def test_escape_entry2_3() { runner.runOneTest("escape_entry2_3") }
  @Test def test_escape_entry2_4() { runner.runOneTest("escape_entry2_4") }
  @Test def test_escape_entry2_5() { runner.runOneTest("escape_entry2_5") }
  @Test def test_escape_entry2_6() { runner.runOneTest("escape_entry2_6") }
  @Test def test_escape_entry2_7() { runner.runOneTest("escape_entry2_7") }
  @Test def test_escape_entry2_8() { runner.runOneTest("escape_entry2_8") }
  @Test def test_escape_entry2_9() { runner.runOneTest("escape_entry2_9") }
  @Test def test_escape_entry2_10() { runner.runOneTest("escape_entry2_10") }
  @Test def test_escape_entry2_11() { runner.runOneTest("escape_entry2_11") }
  @Test def test_escape_entry2_12() { runner.runOneTest("escape_entry2_12") }
  @Test def test_escape_entry2_13() { runner.runOneTest("escape_entry2_13") }
  @Test def test_escape_entry2_14() { runner.runOneTest("escape_entry2_14") }
  @Test def test_escape_entry2_15() { runner.runOneTest("escape_entry2_15") }
  @Test def test_escape_entry2_16() { runner.runOneTest("escape_entry2_16") }
  @Test def test_escape_entry2_17() { runner.runOneTest("escape_entry2_17") }
  @Test def test_escape_entry2_18() { runner.runOneTest("escape_entry2_18") }

  @Test def test_escape_entry3_1() { runner.runOneTest("escape_entry3_1") }
  @Test def test_escape_entry3_2() { runner.runOneTest("escape_entry3_2") }
  @Test def test_escape_entry3_3() { runner.runOneTest("escape_entry3_3") }
  @Test def test_escape_entry3_4() { runner.runOneTest("escape_entry3_4") }
  @Test def test_escape_entry3_5() { runner.runOneTest("escape_entry3_5") }
  @Test def test_escape_entry3_6() { runner.runOneTest("escape_entry3_6") }
  @Test def test_escape_entry3_7() { runner.runOneTest("escape_entry3_7") }
  @Test def test_escape_entry3_8() { runner.runOneTest("escape_entry3_8") }
  @Test def test_escape_entry3_9() { runner.runOneTest("escape_entry3_9") }
  @Test def test_escape_entry3_10() { runner.runOneTest("escape_entry3_10") }
  @Test def test_escape_entry3_11() { runner.runOneTest("escape_entry3_11") }
  @Test def test_escape_entry3_12() { runner.runOneTest("escape_entry3_12") }
  @Test def test_escape_entry3_13() { runner.runOneTest("escape_entry3_13") }
  @Test def test_escape_entry3_14() { runner.runOneTest("escape_entry3_14") }
  @Test def test_escape_entry3_15() { runner.runOneTest("escape_entry3_15") }
  @Test def test_escape_entry3_16() { runner.runOneTest("escape_entry3_16") }
  @Test def test_escape_entry3_17() { runner.runOneTest("escape_entry3_17") }
  @Test def test_escape_entry3_18() { runner.runOneTest("escape_entry3_18") }
  @Test def test_escape_entry3_19() { runner.runOneTest("escape_entry3_19") }
  @Test def test_escape_entry3_20() { runner.runOneTest("escape_entry3_20") }
  @Test def test_escape_entry3_21() { runner.runOneTest("escape_entry3_21") }
  @Test def test_escape_entry3_22() { runner.runOneTest("escape_entry3_22") }
  @Test def test_escape_entry3_23() { runner.runOneTest("escape_entry3_23") }
  @Test def test_escape_entry3_24() { runner.runOneTest("escape_entry3_24") }
  @Test def test_escape_entry3_25() { runner.runOneTest("escape_entry3_25") }
  @Test def test_escape_entry3_26() { runner.runOneTest("escape_entry3_26") }
  @Test def test_escape_entry3_27() { runner.runOneTest("escape_entry3_27") }
  @Test def test_escape_entry3_28() { runner.runOneTest("escape_entry3_28") }
  @Test def test_escape_entry3_29() { runner.runOneTest("escape_entry3_29") }
  @Test def test_escape_entry3_30() { runner.runOneTest("escape_entry3_30") }
  @Test def test_escape_entry3_31() { runner.runOneTest("escape_entry3_31") }

  @Test def test_escape_entry4_1() { runner.runOneTest("escape_entry4_1") }
  @Test def test_escape_entry4_2() { runner.runOneTest("escape_entry4_2") }
  @Test def test_escape_entry4_3() { runner.runOneTest("escape_entry4_3") }
  @Test def test_escape_entry4_4() { runner.runOneTest("escape_entry4_4") }
  @Test def test_escape_entry4_5() { runner.runOneTest("escape_entry4_5") }
  @Test def test_escape_entry4_6() { runner.runOneTest("escape_entry4_6") }
  @Test def test_escape_entry4_7() { runner.runOneTest("escape_entry4_7") }
  @Test def test_escape_entry4_8() { runner.runOneTest("escape_entry4_8") }
  @Test def test_escape_entry4_9() { runner.runOneTest("escape_entry4_9") }
  @Test def test_escape_entry4_10() { runner.runOneTest("escape_entry4_10") }
  @Test def test_escape_entry4_11() { runner.runOneTest("escape_entry4_11") }
  @Test def test_escape_entry4_12() { runner.runOneTest("escape_entry4_12") }
  @Test def test_escape_entry4_13() { runner.runOneTest("escape_entry4_13") }
  @Test def test_escape_entry4_14() { runner.runOneTest("escape_entry4_14") }
  @Test def test_escape_entry4_15() { runner.runOneTest("escape_entry4_15") }
  @Test def test_escape_entry4_16() { runner.runOneTest("escape_entry4_16") }
  @Test def test_escape_entry4_17() { runner.runOneTest("escape_entry4_17") }
  @Test def test_escape_entry4_18() { runner.runOneTest("escape_entry4_18") }
  @Test def test_escape_entry4_19() { runner.runOneTest("escape_entry4_19") }
  @Test def test_escape_entry4_20() { runner.runOneTest("escape_entry4_20") }
  @Test def test_escape_entry4_21() { runner.runOneTest("escape_entry4_21") }
  @Test def test_escape_entry4_22() { runner.runOneTest("escape_entry4_22") }
  @Test def test_escape_entry4_23() { runner.runOneTest("escape_entry4_23") }
  @Test def test_escape_entry4_24() { runner.runOneTest("escape_entry4_24") }
  @Test def test_escape_entry4_25() { runner.runOneTest("escape_entry4_25") }
  @Test def test_escape_entry4_26() { runner.runOneTest("escape_entry4_26") }
  @Test def test_escape_entry4_27() { runner.runOneTest("escape_entry4_27") }

}
