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

package org.apache.daffodil.section12.lengthKind

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestLengthKindDelimited extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/DelimitedTests.tdml"
}

object TestLengthKindDelimitedAB extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/AB.tdml"
}

object TestLengthKindDelimitedAN extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/AN.tdml"
}

class TestLengthKindDelimited extends TdmlTests {
  val tdmlSuite = TestLengthKindDelimited

  @Test def delimited_binary_int_seqSep = test

  @Test def TestDoubleNewLineTerminator = test
  @Test def TestDoubleNewLineSeparator = test
  @Test def TestDoubleNewLineSeparatorBasic = test

  @Test def NumSeq_00a = test
  @Test def NumSeq_00nl = test
  @Test def NumSeq_01 = test
  @Test def nested_NumSeq_01 = test
  @Test def eofTest1 = test
  @Test def NumSeq_03 = test
  @Test def NumSeq_04 = test
  @Test def NumSeq_05 = test
  @Test def NumSeq_06 = test
  @Test def NumSeq_07 = test
  @Test def NumSeq_08 = test
  @Test def NumSeq_09 = test
  // DAFFODIL-230 dfdl:documentFinalTerminatorCanBeMissing
  @Ignore @Test def NumSeq_10 = test
  @Test def delimsCheck = test
  @Test def lengthKindDelimited_01 = test
  @Test def lengthKindDelimited_02 = test
  @Test def lengthKindDelimited_03 = test
  @Test def lengthKindDelimited_04 = test
  @Test def NumSeq_11 = test
  @Test def NumSeq_12 = test
  @Test def NumSeq_13 = test
  // DAFFODIL-1975
  @Ignore @Test def NumSeq_13a = test
  @Test def NumSeq_13Fail = test
  @Test def NumSeq_14 = test
  // Tests that initiator is found when on ElementRef
  @Test def refInitiator = test
  // Tests that initiator is found when on GlobalElmentDecl
  @Test def refInitiator2 = test
  @Test def binary_delimited_fail = test
  @Test def Lesson1_lengthKind_delimited = test
  @Test def Lesson4_delimited_fixed_length = test
  @Test def delimited_construct = test
}

class TestLengthKindDelimitedAB extends TdmlTests {
  val tdmlSuite = TestLengthKindDelimitedAB

  @Test def AB000 = test
  @Test def AB001 = test
  @Test def AB002 = test
  @Test def AB003 = test
  @Test def AB004 = test
  @Test def AB005_parse = test
  @Test def AB005_unparse = test
}

class TestLengthKindDelimitedAN extends TdmlTests {
  val tdmlSuite = TestLengthKindDelimitedAN

  @Test def AN000 = test
  @Test def AN001 = test
}
