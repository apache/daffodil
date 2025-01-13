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

package org.apache.daffodil.section13.packed

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestPacked extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/packed/packed.tdml"
}

class TestPacked extends TdmlTests {
  val tdmlSuite = TestPacked

  @Test def hexCharset01 = test
  @Test def hexCharset02 = test
  // textNumberPattern V symbol - DAFFODIL-853
  @Ignore @Test def hexCharset03 = test
  @Test def hexCharset04 = test

  @Test def packedCharset01 = test
  @Test def packedCharset02 = test
  @Test def packedCharset03 = test
  @Test def packedCharset04 = test
  @Test def packedCharset05 = test
  @Test def packedCharset06 = test
  @Test def packedCharset07 = test
  @Test def packedCharset08 = test
  @Test def packedCharset09 = test
  @Test def packedCharset10 = test

  @Test def zeroLengthPackedCharset = test
  @Test def runtimeLengthPackedCharset1 = test
  @Test def runtimeLengthPackedCharset2 = test

  @Test def bcdCharset01 = test
  @Test def bcdCharset02 = test
  @Test def bcdCharset03 = test
  @Test def bcdCharset04 = test
  @Test def bcdCharset05 = test
  @Test def bcdCharset06 = test
  @Test def bcdCharset07 = test
  @Test def bcdCharset08 = test
  @Test def bcdCharset09 = test
  @Test def bcdCharset10 = test
  @Test def bcdCharset11 = test
  @Test def bcdCharset12 = test
  @Test def bcdCharset13 = test

  @Test def packedNegativeUnsigned = test
  @Test def packedIntTooLarge = test
  @Test def packedIntMax = test

  @Test def IBM4690Charset01 = test
  @Test def IBM4690Charset02 = test
  @Test def IBM4690Charset03 = test
  @Test def IBM4690Charset04 = test
  @Test def IBM4690Charset05 = test
  @Test def IBM4690Charset06 = test
  @Test def IBM4690Charset07 = test
  @Test def IBM4690Charset08 = test
  @Test def IBM4690Charset09 = test
  @Test def IBM4690Charset10 = test

  @Test def DelimitedPackedIntSeq = test
  @Test def DelimitedPackedDecSeq = test
  @Test def DelimitedPackedIntSeqUnparser = test
  @Test def DelimitedPackedDecSeqUnparser = test
  @Test def DelimitedBCDIntSeq = test
  @Test def DelimitedBCDDecSeq = test
  @Test def DelimitedBCDIntSeqUnparser = test
  @Test def DelimitedBCDDecSeqUnparser = test
  @Test def DelimitedIBM4690IntSeq = test
  @Test def DelimitedIBM4690DecSeq = test
  @Test def DelimitedIBM4690IntSeqUnparser = test
  @Test def DelimitedIBM4690DecSeqUnparser = test

  // Daffodil-2961
  @Test def bcdBigIntToLongExpr = test
}
