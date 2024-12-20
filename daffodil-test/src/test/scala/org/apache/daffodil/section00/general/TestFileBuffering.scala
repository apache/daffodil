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

package org.apache.daffodil.section00.general

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestUnparserFileBuffering extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section00/general/testUnparserFileBuffering.tdml"
}

class TestUnparserFileBuffering extends TdmlTests {
  val tdmlSuite = TestUnparserFileBuffering

  @Test def puaInfosetChars_01_ffb = test
  @Test def puaInfosetChars_02_ffb = test

  @Test def unparseFixedLengthString01_ffb = test
  @Test def unparseFixedLengthString02_ffb = test
  @Test def unparseFixedLengthString03_ffb = test

  @Test def parseFixedLengthString01_ffb = test
  @Test def parseFixedLengthStringLength0_ffb = test

  @Test def negativeUnparseTest01_ffb = test
  @Test def negativeUnparseTest02_ffb = test
  @Test def negativeUnparseTest03_ffb = test
  @Test def negativeUnparseTest04_ffb = test
  @Test def negativeUnparseTest05_ffb = test

  @Test def unparseDelimitedString01_ffb = test
  @Test def unparseDelimitedString02_ffb = test
  @Test def unparseDelimitedString03_ffb = test
  @Test def unparseDelimitedString04_ffb = test
  @Test def unparseDelimitedString05_ffb = test
  @Test def unparseDelimitedString06_ffb = test
  @Test def unparseDelimitedString07_ffb = test

  @Test def parseDelimitedString01_ffb = test

  // DFDL-1650
  @Test def alignmentPaddingOVC1_ffb = test
  @Test def alignmentPaddingOVC2_ffb = test
  @Test def alignmentPaddingOVC3_ffb = test
  @Test def alignmentPaddingOVC4_ffb = test
}
