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

object TestUnparserGeneral extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section00/general/testUnparserGeneral.tdml"
}

class TestUnparserGeneral extends TdmlTests {
  val tdmlSuite = TestUnparserGeneral

  @Test def apostrophe_01 = test

  @Test def puaPreexistingInfosetChars = test
  @Test def puaPreexistingInfosetChars_remapped = test
  @Test def variableWidthComplexType = test

  @Test def puaInfosetChars_CR_CRLF_01 = test

  @Test def puaInfosetChars_CR_CRLF_02 = test

  @Test def puaInfosetChars_01 = test
  @Test def puaInfosetChars_02 = test

  @Test def unparseFixedLengthString01 = test
  @Test def unparseFixedLengthString02 = test
  @Test def unparseFixedLengthString03 = test
  @Test def unparseFixedLengthString04 = test
  @Test def unparseFixedLengthString05 = test
  @Test def unparseFixedLengthString06 = test
  @Test def unparseFixedLengthString07 = test

  @Test def parseFixedLengthString01 = test
  @Test def parseFixedLengthStringLength0 = test

  @Test def negativeUnparseTest01 = test
  @Test def negativeUnparseTest02 = test
  @Test def negativeUnparseTest03 = test
  @Test def negativeUnparseTest04 = test
  @Test def negativeUnparseTest05 = test

  @Test def unparseDelimitedString01 = test
  @Test def unparseDelimitedString02 = test
  @Test def unparseDelimitedString03 = test
  @Test def unparseDelimitedString04 = test
  @Test def unparseDelimitedString05 = test
  @Test def unparseDelimitedString06 = test
  @Test def unparseDelimitedString07 = test

  @Test def parseDelimitedString01 = test

  // DFDL-1650
  @Test def alignmentPaddingOVC1 = test
  @Test def alignmentPaddingOVC2 = test
  @Test def alignmentPaddingOVC3 = test
  @Test def alignmentPaddingOVC4 = test

  // DFDL-1589
  @Test def emptyOutputNewLine1 = test
}
