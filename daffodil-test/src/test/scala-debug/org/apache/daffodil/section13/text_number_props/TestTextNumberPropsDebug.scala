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

package org.apache.daffodil.section13.text_number_props

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestTextNumberPropsDebug {
  val testDir = "/org/apache/daffodil/section13/text_number_props/"
  val runner = Runner(testDir, "TextNumberProps.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }
}

class TestTextNumberPropsDebug {

  import TestTextNumberPropsDebug._

  // DFDL-847
  @Test def test_textStandardDecimalSeparator10() { runner.runOneTest("textStandardDecimalSeparator10") }
  @Test def test_textStandardDecimalSeparator11() { runner.runOneTest("textStandardDecimalSeparator11") }

  // DFDL-853
  @Test def test_textNumberPattern_pSymbol01() { runner.runOneTest("textNumberPattern_pSymbol01") }
  @Test def test_textNumberPattern_pSymbol02() { runner.runOneTest("textNumberPattern_pSymbol02") }

  // DFDL-861
  @Test def test_infnanCaseInsensitive() { runner.runOneTest("infnanCaseInsensitive") }
  @Test def test_expCaseSensitive() { runner.runOneTest("expCaseSensitive") }

  // DAFFODIL-1981
  @Test def test_expEmptyString() { runner.runOneTest("expEmptyString") }
  @Test def test_expEmptyString2() { runner.runOneTest("expEmptyString2") }
}
