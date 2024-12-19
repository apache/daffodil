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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestNillableAA extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/nillable/nillable.tdml"
}

object TestNillableLN extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/nillable/literal-value-nils.tdml"
}

object TestNillableLC extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/nillable/literal-character-nils.tdml"
}

object TestNillableEntities extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section06/entities/entities_01.tdml"
}

class TestNillableAA extends TdmlTests {
  val tdmlSuite = TestNillableAA

  @Test def litNil1 = test
  @Test def litNil2 = test
  @Test def litNil3 = test
  @Test def litNil4 = test
  @Test def litNil4b = test
  @Test def litNil5 = test
  @Test def litNil6 = test
  @Test def litNil7 = test
  @Test def missing_scalar = test
  @Test def nillable1 = test
  @Test def edifact1a = test
  @Test def complexNillable_01 = test
  @Test def complexNillable_02 = test
}

class TestNillableLN extends TdmlTests {
  val tdmlSuite = TestNillableLN

  @Test def test_complex_nil = test
  @Test def text_nil_characterClass_04_parse = test
  @Test def text_03 = test
  @Test def text_03ic = test
  @Test def text_04 = test
  @Test def text_05 = test
  @Test def text_06 = test
  @Test def binary_01 = test
  @Test def test_padded_nils = test
  @Test def nillable_ovc_01 = test
}

class TestNillableLC extends TdmlTests {
  val tdmlSuite = TestNillableLC

  /* These should demonstrate that:
   *  DFDL Char Classes are not allowed for literalCharacter
   *  DFDL Char Entities are allowed for literalCharacter
   *  Raw bytes entities are allowed for literalCharacter
   *  Only 1 character or byte are allowed for literalCharacter
   *
   *  According to analysis doc, should also work for numeric
   *  and hex entities.
   * */
  @Test def text_01 = test
  @Test def text_01ic = test
  @Test def text_02 = test
  @Test def text_03 = test
  @Test def text_04 = test
  @Test def binary_01 = test
}

class TestNillableEntities extends TdmlTests {
  val tdmlSuite = TestNillableEntities

  @Test def entity_fail_05 = test
  @Test def entity_fail_06 = test
  @Test def entity_success_05 = test
  @Test def entity_success_06 = test
}
