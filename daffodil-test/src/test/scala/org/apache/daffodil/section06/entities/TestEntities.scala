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

package org.apache.daffodil.section06.entities

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEntities {
  private val testDir = "/org/apache/daffodil/section06/entities/"

  val runner = Runner(testDir, "charClassEntities.tdml")
  val runner_01 = Runner(testDir, "Entities.tdml")
  val runnerEntity = Runner(testDir, "entities_01.tdml")
  val runnerInvalid = Runner(testDir, "InvalidEntities.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner_01.reset
    runnerEntity.reset
    runnerInvalid.reset
  }
}
class TestEntities {
  import TestEntities._

  @Test def test_doubleNL2(): Unit = { runner.runOneTest("doubleNL2") }

  @Test def test_entityInError(): Unit = { runner.runOneTest("entityInError") }
  @Test def test_LineFeed(): Unit = { runner.runOneTest("LineFeed") }
  @Test def test_CarriageReturn(): Unit = { runner.runOneTest("CarriageReturn") }
  @Test def test_LineSeparator(): Unit = { runner.runOneTest("LineSeparator") }
  @Test def test_NextLine(): Unit = { runner.runOneTest("NextLine") }
  @Test def test_LineFeed_byte(): Unit = { runner.runOneTest("LineFeed_byte") }
  @Test def test_CarriageReturn_byte(): Unit = { runner.runOneTest("CarriageReturn_byte") }
  @Test def test_CRLF_byte(): Unit = { runner.runOneTest("CRLF_byte") }
  @Test def test_LineSeparator_byte(): Unit = { runner.runOneTest("LineSeparator_byte") }
  @Test def test_NextLine_byte(): Unit = { runner.runOneTest("NextLine_byte") }
  @Test def test_FormFeed(): Unit = { runner.runOneTest("FormFeed") }
  @Test def test_HexCodePoint(): Unit = { runner.runOneTest("HexCodePoint") }

  @Test def test_entityAndNonMix_01(): Unit = { runner_01.runOneTest("entityAndNonMix_01") }
  @Test def test_entityAndNonMix_02(): Unit = { runner_01.runOneTest("entityAndNonMix_02") }
  @Test def test_entityAndNonMix_03(): Unit = { runner_01.runOneTest("entityAndNonMix_03") }
  @Test def test_entityAndNonMix_04(): Unit = { runner_01.runOneTest("entityAndNonMix_04") }

  // DFDL-378
  //  @Test def test_dataDumpEncoding() { runner_01.runOneTest("dataDumpEncoding") }
  @Test def test_errorEncoding(): Unit = { runner_01.runOneTest("errorEncoding") }

  @Test def test_doubleNLterminator(): Unit = { runner_01.runOneTest("doubleNLterminator") }
  @Test def test_doubleNLseparator(): Unit = { runner_01.runOneTest("doubleNLseparator") }

  @Test def test_text_entities_6_02(): Unit = { runner_01.runOneTest("text_entities_6_02") }
  @Test def test_text_entities_6_03(): Unit = { runner_01.runOneTest("text_entities_6_03") }
  @Test def test_text_entities_6_03b(): Unit = { runner_01.runOneTest("text_entities_6_03b") }
  @Test def test_text_entities_6_04(): Unit = { runner_01.runOneTest("text_entities_6_04") }
  @Test def test_byte_entities_6_01(): Unit = { runner_01.runOneTest("byte_entities_6_01") }
  @Test def test_byte_entities_6_02(): Unit = { runner_01.runOneTest("byte_entities_6_02") }
  @Test def test_byte_entities_6_03(): Unit = { runner_01.runOneTest("byte_entities_6_03") }
  @Test def test_byte_entities_6_04(): Unit = { runner_01.runOneTest("byte_entities_6_04") }
  @Test def test_byte_entities_6_05(): Unit = { runner_01.runOneTest("byte_entities_6_05") }
  @Test def test_byte_entities_6_06(): Unit = { runner_01.runOneTest("byte_entities_6_06") }
  @Test def test_byte_entities_6_07(): Unit = { runner_01.runOneTest("byte_entities_6_07") }
  @Test def test_byte_entities_6_08(): Unit = { runner_01.runOneTest("byte_entities_6_08") }
  // DAFFODIL-2102
  //  @Test def test_byte_entities_6_10() { runner_01.runOneTest("byte_entities_6_10") }

  @Test def test_whitespace_01(): Unit = { runner_01.runOneTest("whitespace_01") }
  @Test def test_whitespace_02(): Unit = { runner_01.runOneTest("whitespace_02") }
  @Test def test_whitespace_03(): Unit = { runner_01.runOneTest("whitespace_03") }
  @Test def test_whitespace_04(): Unit = { runner_01.runOneTest("whitespace_04") }
  @Test def test_whitespace_05(): Unit = { runner_01.runOneTest("whitespace_05") }
  @Test def test_whitespace_06(): Unit = { runner_01.runOneTest("whitespace_06") }
  @Test def test_whitespace_07(): Unit = { runner_01.runOneTest("whitespace_07") }
  @Test def test_whitespace_08(): Unit = { runner_01.runOneTest("whitespace_08") }
  @Test def test_whitespace_09(): Unit = { runner_01.runOneTest("whitespace_09") }
  @Test def test_whitespace_10(): Unit = { runner_01.runOneTest("whitespace_10") }

  // DAFFODIL-1475
  @Test def test_emptyStringEntityTermInExpression_01(): Unit = { runner_01.runOneTest("emptyStringEntityTermInExpression_01") }
  @Test def test_emptyStringEntityTermInExpression_02(): Unit = { runner_01.runOneTest("emptyStringEntityTermInExpression_02") }
  @Test def test_emptyStringEntityTermInExpressionDelimited_01(): Unit = { runner_01.runOneTest("emptyStringEntityTermInExpressionDelimited_01") }
  @Test def test_emptyStringEntityTermInComplex_01(): Unit = { runner_01.runOneTest("emptyStringEntityTermInComplex_01") }
  @Test def test_emptyStringEntityTermInComplex_02(): Unit = { runner_01.runOneTest("emptyStringEntityTermInComplex_02") }

  @Test def test_emptyStringEntityInitiator_01(): Unit = { runner_01.runOneTest("emptyStringEntityInitiator_01") }
  @Test def test_emptyStringEntityInitiator_02(): Unit = { runner_01.runOneTest("emptyStringEntityInitiator_02") }
  @Test def test_emptyStringEntityInitiator_03(): Unit = { runner_01.runOneTest("emptyStringEntityInitiator_03") }

  @Test def test_entity_fail_01(): Unit = { runnerEntity.runOneTest("entity_fail_01") }
  @Test def test_entity_fail_02(): Unit = { runnerEntity.runOneTest("entity_fail_02") }

  // DAFFODIL-1477
  @Test def test_entity_fail_03a(): Unit = { runnerEntity.runOneTest("entity_fail_03a") }
  @Test def test_entity_fail_03b(): Unit = { runnerEntity.runOneTest("entity_fail_03b") }
  @Test def test_entity_fail_04(): Unit = { runnerEntity.runOneTest("entity_fail_04") }

  @Test def test_invalid_entity_01(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_name") }
  @Test def test_invalid_entity_02(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_decimalCodePoint") }
  @Test def test_invalid_entity_03(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_hexaDecimalCodePoint") }
  @Test def test_invalid_entity_04(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_rawBytes") }
  @Test def test_invalid_entity_05(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_among_multiple_valid") }
  @Test def test_invalid_entity_06(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_among_multiple_valid_combined") }
  @Test def test_invalid_entity_07(): Unit = { runnerInvalid.runOneTest("text_invalid_entity_escaped") }

  @Test def test_allAsciiHexEntities(): Unit = { runner_01.runOneTest("allAsciiHexEntities") }
  @Test def test_allAsciiDecEntities(): Unit = { runner_01.runOneTest("allAsciiDecEntities") }
}
