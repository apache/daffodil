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

package org.apache.daffodil.section10.representation_properties

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestRepProps2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section10/representation_properties/encodings.tdml"
}

class TestRepProps2 extends TdmlTests {
  val tdmlSuite = TestRepProps2

  @Test def ebcdic1 = test
  @Test def bits1 = test
  @Test def bits1a = test
  @Test def bits2 = test
  @Test def bits2a = test

  @Test def bitsTerm1 = test

  // fails Left-over data byte 1 limit(bytes) 2
  @Test def bitsTerm2 = test
  @Test def bitsTerm3 = test

  @Test def fiveBitDFI1661DUI001 = test
  @Test def fiveBitDFI1661DUI001_roundTrip = test

  @Test def sixBit1 = test

  @Test def iso88591msbbitsmisaligned = test
  @Test def iso88591lsbbitsmisaligned = test

  @Test def unalignedCharsetWithMandatory8BitAlignment = test

  @Test def automaticAlignedCharsetWithMandatory8BitAlignment = test
}
