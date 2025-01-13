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

object TestNillableUnparseLN extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/nillable/literal-value-nils-unparse.tdml"
}

object TestNillableUnparseLC extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section13/nillable/literal-character-nils-unparse.tdml"
}

class TestNillableUnparseLN extends TdmlTests {
  val tdmlSuite = TestNillableUnparseLN

  @Test def scalar_nonDefaultable_nillable = test
  @Test def scalar_nonDefaultable_nillable_02 = test
  @Test def scalar_nonDefaultable_nillable_03 = test

  @Test def text_complex_nil = test
  @Test def text_complex_nil2 = test
  @Test def text_complex_nil3 = test
  @Test def text_complex_nil4 = test

  @Test def text_nil_only1 = test
  @Test def text_nil_only2 = test
  @Test def text_nil_only3 = test
  @Test def text_nil_only4 = test
  @Test def text_nil_only5 = test
  @Test def text_nil_only6 = test
  @Test def text_nil_only7 = test
  @Test def text_nil_only8 = test
  @Test def text_nil_only9 = test
  @Test def text_nil_only10 = test
  @Test def text_nil_only11 = test
  @Test def text_nil_only12 = test
  @Test def text_nil_only13 = test
  @Test def text_nil_only14 = test
  @Test def text_nil_only15 = test
  @Test def text_nil_only16 = test
  @Test def text_nil_only17 = test

  @Test def text_nil_characterClass_01 = test
  @Test def text_nil_characterClass_02 = test
  @Test def text_nil_characterClass_03 = test
  @Test def text_nil_characterClass_04 = test
  @Test def text_nil_characterClass_05 = test
  @Test def text_nil_characterClass_06 = test
}

class TestNillableUnparseLC extends TdmlTests {
  val tdmlSuite = TestNillableUnparseLC

  @Test def text_01 = test
  @Test def text_01a = test
}
