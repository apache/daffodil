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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestTextNumberPropsUnparse extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section13/text_number_props/TextNumberPropsUnparse.tdml"
}

class TestTextNumberPropsUnparse extends TdmlTests {
  val tdmlSuite = TestTextNumberPropsUnparse

  @Test def unparseDelimitedPaddedString01 = test
  @Test def unparseDelimitedPaddedString02 = test
  @Test def unparseDelimitedPaddedString03 = test
  @Test def unparseDelimitedPaddedString04 = test
  @Test def unparseDelimitedPaddedString05 = test
  @Test def unparseDelimitedPaddedString06 = test
  @Test def unparseDelimitedPaddedString07 = test
  @Test def unparseDelimitedPaddedString08 = test
  @Test def unparseDelimitedPaddedString09 = test
  @Test def unparseDelimitedPaddedString11 = test
  @Test def unparseDelimitedPaddedString12 = test
  @Test def unparseDelimitedPaddedString13 = test
  @Test def unparseDelimitedPaddedString14 = test

  @Test def unparsePaddedString10 = test
  @Test def unparsePaddedString11 = test
  @Test def unparsePaddedString12 = test

  @Test def unparsePaddedStringTruncate01 = test
  @Test def unparsePaddedStringTruncate02 = test
  @Test def unparsePaddedStringTruncate03 = test
  @Test def unparsePaddedStringTruncate04 = test
  @Test def unparsePaddedStringTruncate05 = test
  @Test def unparsePaddedStringTruncate06 = test

  @Test def parseDelimitedPaddedString01 = test

  @Test def unparse_int_01 = test
  @Test def parse_int_01 = test

  @Test def unparse_tnp_01 = test
  @Test def unparse_tnp_02 = test
  @Test def unparse_tnp_03 = test
  @Test def unparse_tnp_04 = test
  @Test def unparse_tnp_05a = test
  @Test def unparse_tnp_05b = test

  @Test def textStandardZeroRepNotDefinedByDefault = test

  @Test def textStandardZeroRep1 = test

  @Test def textStandardZeroRep2 = test
}
