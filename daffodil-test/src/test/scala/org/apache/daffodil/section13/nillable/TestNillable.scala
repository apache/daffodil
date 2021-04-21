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
import org.junit.AfterClass

object TestNillable {
  val testDir = "/org/apache/daffodil/section13/nillable/"
  val testDir_01 = "/org/apache/daffodil/section06/entities/"

  val runnerAA = Runner(testDir, "nillable.tdml")
  val runnerLN = Runner(testDir, "literal-value-nils.tdml")
  val runnerLC = Runner(testDir, "literal-character-nils.tdml")
  val runnerEntity = Runner(testDir_01, "entities_01.tdml")

  @AfterClass def shutDown(): Unit = {
    runnerAA.reset
    runnerLN.reset
    runnerEntity.reset
    runnerLC.reset
  }

}

class TestNillable {

  import TestNillable._

  @Test def test_complex_nil(): Unit = { runnerLN.runOneTest("test_complex_nil") }

  @Test def test_litNil1(): Unit = { runnerAA.runOneTest("litNil1") }
  @Test def test_litNil2(): Unit = { runnerAA.runOneTest("litNil2") }
  @Test def test_litNil3(): Unit = { runnerAA.runOneTest("litNil3") }
  @Test def test_litNil4(): Unit = { runnerAA.runOneTest("litNil4") }
  @Test def test_litNil4b(): Unit = { runnerAA.runOneTest("litNil4b") }
  @Test def test_litNil5(): Unit = { runnerAA.runOneTest("litNil5") }
  @Test def test_litNil6(): Unit = { runnerAA.runOneTest("litNil6") }
  @Test def test_litNil7(): Unit = { runnerAA.runOneTest("litNil7") }
  @Test def test_missing_scalar(): Unit = { runnerAA.runOneTest("missing_scalar") }
  @Test def test_nillable1(): Unit = { runnerAA.runOneTest("nillable1") }
  @Test def test_edifact1a(): Unit = { runnerAA.runOneTest("edifact1a") }

  @Test def test_text_nil_characterClass_04_parse(): Unit = { runnerLN.runOneTest("text_nil_characterClass_04_parse") }

  @Test def test_text_03(): Unit = { runnerLN.runOneTest("text_03") }
  @Test def test_text_03ic(): Unit = { runnerLN.runOneTest("text_03ic") }
  @Test def test_text_04(): Unit = { runnerLN.runOneTest("text_04") }
  @Test def test_text_05(): Unit = { runnerLN.runOneTest("text_05") }
  @Test def test_text_06(): Unit = { runnerLN.runOneTest("text_06") }
  @Test def test_binary_01(): Unit = { runnerLN.runOneTest("binary_01") }
  @Test def test_padded_nils(): Unit = { runnerLN.runOneTest("test_padded_nils") }

  @Test def test_nillable_ovc_01(): Unit = { runnerLN.runOneTest("nillable_ovc_01") }

  /* These should demonstrate that:
   *   DFDL Char Classes are not allowed for literalCharacter
   *  DFDL Char Entities are allowed for literalCharacter
   *  Raw bytes entities are allowed for literalCharacter
   *  Only 1 character or byte are allowed for literalCharacter
   *
   *  According to analysis doc, should also work for numeric
   *  and hex entities.
   * */
  @Test def test_text_lit_char_01(): Unit = { runnerLC.runOneTest("text_01") }
  @Test def test_text_lit_char_01ic(): Unit = { runnerLC.runOneTest("text_01ic") }
  @Test def test_text_lit_char_02(): Unit = { runnerLC.runOneTest("text_02") }
  @Test def test_text_lit_char_03(): Unit = { runnerLC.runOneTest("text_03") }
  @Test def test_text_lit_char_04(): Unit = { runnerLC.runOneTest("text_04") }
  @Test def test_binary_lit_char_01(): Unit = { runnerLC.runOneTest("binary_01") }

  @Test def test_entity_fail_05(): Unit = { runnerEntity.runOneTest("entity_fail_05") }
  @Test def test_entity_fail_06(): Unit = { runnerEntity.runOneTest("entity_fail_06") }
  @Test def test_entity_success_05(): Unit = { runnerEntity.runOneTest("entity_success_05") }
  @Test def test_entity_success_06(): Unit = { runnerEntity.runOneTest("entity_success_06") }

}
