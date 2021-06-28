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

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestPacked {
  val testDir = "/org/apache/daffodil/section13/packed/"
  val runner = Runner(testDir, "packed.tdml")

  @AfterClass def shutdown(): Unit = {
    runner.reset
  }

}

class TestPacked {
  import TestPacked._

  @Test def testHexCharset01(): Unit = { runner.runOneTest("hexCharset01") }
  @Test def testHexCharset02(): Unit = { runner.runOneTest("hexCharset02") }
  // @Test def testHexCharset03(): Unit = { runner.runOneTest("hexCharset03") } // textNumberPattern V symbol - DAFFODIL-853
  @Test def testHexCharset04(): Unit = { runner.runOneTest("hexCharset04") }

  @Test def testPackedCharset01(): Unit = { runner.runOneTest("packedCharset01") }
  @Test def testPackedCharset02(): Unit = { runner.runOneTest("packedCharset02") }
  @Test def testPackedCharset03(): Unit = { runner.runOneTest("packedCharset03") }
  @Test def testPackedCharset04(): Unit = { runner.runOneTest("packedCharset04") }
  @Test def testPackedCharset05(): Unit = { runner.runOneTest("packedCharset05") }
  @Test def testPackedCharset06(): Unit = { runner.runOneTest("packedCharset06") }
  @Test def testPackedCharset07(): Unit = { runner.runOneTest("packedCharset07") }
  @Test def testPackedCharset08(): Unit = { runner.runOneTest("packedCharset08") }
  @Test def testPackedCharset09(): Unit = { runner.runOneTest("packedCharset09") }
  @Test def testPackedCharset10(): Unit = { runner.runOneTest("packedCharset10") }

  @Test def testBCDCharset01(): Unit = { runner.runOneTest("bcdCharset01") }
  @Test def testBCDCharset02(): Unit = { runner.runOneTest("bcdCharset02") }
  @Test def testBCDCharset03(): Unit = { runner.runOneTest("bcdCharset03") }
  @Test def testBCDCharset04(): Unit = { runner.runOneTest("bcdCharset04") }
  @Test def testBCDCharset05(): Unit = { runner.runOneTest("bcdCharset05") }
  @Test def testBCDCharset06(): Unit = { runner.runOneTest("bcdCharset06") }
  @Test def testBCDCharset07(): Unit = { runner.runOneTest("bcdCharset07") }
  @Test def testBCDCharset08(): Unit = { runner.runOneTest("bcdCharset08") }
  @Test def testBCDCharset09(): Unit = { runner.runOneTest("bcdCharset09") }
  @Test def testBCDCharset10(): Unit = { runner.runOneTest("bcdCharset10") }
  @Test def testBCDCharset11(): Unit = { runner.runOneTest("bcdCharset11") }
  @Test def testBCDCharset12(): Unit = { runner.runOneTest("bcdCharset12") }
  @Test def testBCDCharset13(): Unit = { runner.runOneTest("bcdCharset13") }

  @Test def testIBM4690Charset01(): Unit = { runner.runOneTest("IBM4690Charset01") }
  @Test def testIBM4690Charset02(): Unit = { runner.runOneTest("IBM4690Charset02") }
  @Test def testIBM4690Charset03(): Unit = { runner.runOneTest("IBM4690Charset03") }
  @Test def testIBM4690Charset04(): Unit = { runner.runOneTest("IBM4690Charset04") }
  @Test def testIBM4690Charset05(): Unit = { runner.runOneTest("IBM4690Charset05") }
  @Test def testIBM4690Charset06(): Unit = { runner.runOneTest("IBM4690Charset06") }
  @Test def testIBM4690Charset07(): Unit = { runner.runOneTest("IBM4690Charset07") }
  @Test def testIBM4690Charset08(): Unit = { runner.runOneTest("IBM4690Charset08") }
  @Test def testIBM4690Charset09(): Unit = { runner.runOneTest("IBM4690Charset09") }
  @Test def testIBM4690Charset10(): Unit = { runner.runOneTest("IBM4690Charset10") }

  @Test def testDelimitedPackedIntSeq(): Unit = { runner.runOneTest("DelimitedPackedIntSeq") }
  @Test def testDelimitedPackedDecSeq(): Unit = { runner.runOneTest("DelimitedPackedDecSeq") }
  @Test def testDelimitedPackedIntSeqUnparser(): Unit = { runner.runOneTest("DelimitedPackedIntSeqUnparser") }
  @Test def testDelimitedPackedDecSeqUnparser(): Unit = { runner.runOneTest("DelimitedPackedDecSeqUnparser") }
  @Test def testDelimitedBCDIntSeq(): Unit = { runner.runOneTest("DelimitedBCDIntSeq") }
  @Test def testDelimitedBCDDecSeq(): Unit = { runner.runOneTest("DelimitedBCDDecSeq") }
  @Test def testDelimitedBCDIntSeqUnparser(): Unit = { runner.runOneTest("DelimitedBCDIntSeqUnparser") }
  @Test def testDelimitedBCDDecSeqUnparser(): Unit = { runner.runOneTest("DelimitedBCDDecSeqUnparser") }
  @Test def testDelimitedIBM4690IntSeq(): Unit = { runner.runOneTest("DelimitedIBM4690IntSeq") }
  @Test def testDelimitedIBM4690DecSeq(): Unit = { runner.runOneTest("DelimitedIBM4690DecSeq") }
  @Test def testDelimitedIBM4690IntSeqUnparser(): Unit = { runner.runOneTest("DelimitedIBM4690IntSeqUnparser") }
  @Test def testDelimitedIBM4690DecSeqUnparser(): Unit = { runner.runOneTest("DelimitedIBM4690DecSeqUnparser") }
}
