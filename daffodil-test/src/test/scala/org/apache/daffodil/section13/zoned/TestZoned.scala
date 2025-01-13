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

package org.apache.daffodil.section13.zoned

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestZoned extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/zoned/zoned.tdml"
}

object TestZoned2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/zoned/zoned2.tdml"
}

class TestZoned extends TdmlTests {
  val tdmlSuite = TestZoned

  @Test def zoned_textNumberPattern_fail01 = test
  @Test def zoned_textNumberPattern_fail02 = test
  @Test def zoned_textNumberPattern_fail03 = test
  @Test def zoned_textNumberPattern_fail04 = test
  @Test def zoned_textNumberPattern_fail05 = test
  @Test def zoned_textNumberPattern_fail06 = test
  @Test def zoned_float_fail01 = test
  @Test def zoned_double_fail01 = test

  @Test def ZonedStandard01 = test
  @Test def ZonedStandard02 = test
  @Test def ZonedStandard03 = test
  @Test def ZonedStandard04 = test
  @Test def ZonedStandard05 = test

  @Test def ZonedTranslatedEBCDIC01 = test
  @Test def ZonedTranslatedEBCDIC02 = test
  @Test def ZonedTranslatedEBCDIC03 = test
  @Test def ZonedTranslatedEBCDIC04 = test
  @Test def ZonedTranslatedEBCDIC05 = test

  @Test def ZonedCARealiaModified01 = test
  @Test def ZonedCARealiaModified02 = test
  @Test def ZonedCARealiaModified03 = test
  @Test def ZonedCARealiaModified04 = test
  @Test def ZonedCARealiaModified05 = test

  @Test def ZonedTandemModified01 = test
  @Test def ZonedTandemModified02 = test
  @Test def ZonedTandemModified03 = test
  @Test def ZonedTandemModified04 = test
  @Test def ZonedTandemModified05 = test
}

class TestZoned2 extends TdmlTests {
  val tdmlSuite = TestZoned2

  @Test def ZonedEBCDICLeadingOverpunchedSign = test

  @Test def ZonedEBCDICLeadingOverpunchedSign_B5 = test

  @Test def ZonedEBCDICLeadingOverpunchedSignBadDigit = test
  @Test def ZonedEBCDICTrailingOverpunchedSign = test

  @Test def ZonedEBCDICTrailingOverpunchedSign_B2 = test
  @Test def ZonedEBCDICTrailingOverpunchedSignBadDigit = test
}
