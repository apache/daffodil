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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestCharClassEntities extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/entities/charClassEntities.tdml"
}

object TestEntities extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/entities/Entities.tdml"
}

object TestEntities01 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/entities/entities_01.tdml"
}

object TestInvalidEntities extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/entities/InvalidEntities.tdml"
}

class TestCharClassEntities extends TdmlTests {
  val tdmlSuite = TestCharClassEntities

  @Test def doubleNL2 = test

  @Test def entityInError = test
  @Test def LineFeed = test
  @Test def CarriageReturn = test
  @Test def LineSeparator = test
  @Test def NextLine = test
  @Test def LineFeed_byte = test
  @Test def CarriageReturn_byte = test
  @Test def CRLF_byte = test
  @Test def LineSeparator_byte = test
  @Test def NextLine_byte = test
  @Test def FormFeed = test
  @Test def HexCodePoint = test
}

class TestEntities extends TdmlTests {
  val tdmlSuite = TestEntities

  @Test def entityAndNonMix_01 = test
  @Test def entityAndNonMix_02 = test
  @Test def entityAndNonMix_03 = test
  @Test def entityAndNonMix_04 = test

  // DFDL-378
  @Ignore @Test def dataDumpEncoding = test
  @Test def errorEncoding = test

  @Test def doubleNLterminator = test
  @Test def doubleNLseparator = test

  @Test def text_entities_6_02 = test
  @Test def text_entities_6_03 = test
  @Test def text_entities_6_03b = test
  @Test def text_entities_6_04 = test
  @Test def byte_entities_6_01 = test
  @Test def byte_entities_6_02 = test
  @Test def byte_entities_6_03 = test
  @Test def byte_entities_6_04 = test
  @Test def byte_entities_6_05 = test
  @Test def byte_entities_6_06 = test
  @Test def byte_entities_6_07 = test
  @Test def byte_entities_6_08 = test
  // DAFFODIL-2102
  @Ignore @Test def byte_entities_6_10 = test

  @Test def whitespace_01 = test
  @Test def whitespace_02 = test
  @Test def whitespace_03 = test
  @Test def whitespace_04 = test
  @Test def whitespace_05 = test
  @Test def whitespace_06 = test
  @Test def whitespace_07 = test
  @Test def whitespace_08 = test
  @Test def whitespace_09 = test
  @Test def whitespace_10 = test

  // DAFFODIL-1475
  @Test def emptyStringEntityTermInExpression_01 = test
  @Test def emptyStringEntityTermInExpression_02 = test
  @Test def emptyStringEntityTermInExpressionDelimited_01 = test
  @Test def emptyStringEntityTermInComplex_01 = test
  @Test def emptyStringEntityTermInComplex_02 = test

  @Test def emptyStringEntityInitiator_01 = test
  @Test def emptyStringEntityInitiator_02 = test
  @Test def emptyStringEntityInitiator_03 = test

  @Test def allAsciiHexEntities = test
  @Test def allAsciiDecEntities = test
}

class TestEntities01 extends TdmlTests {
  val tdmlSuite = TestEntities01

  @Test def entity_fail_01 = test
  @Test def entity_fail_02 = test

  // DAFFODIL-1477
  @Test def entity_fail_03a = test
  @Test def entity_fail_03b = test
  @Test def entity_fail_04 = test
}

class TestInvalidEntities extends TdmlTests {
  val tdmlSuite = TestInvalidEntities

  @Test def text_invalid_entity_name = test
  @Test def text_invalid_entity_decimalCodePoint = test
  @Test def text_invalid_entity_hexaDecimalCodePoint = test
  @Test def text_invalid_entity_rawBytes = test
  @Test def text_invalid_entity_among_multiple_valid = test
  @Test def text_invalid_entity_among_multiple_valid_combined = test
  @Test def text_invalid_entity_escaped = test
}
