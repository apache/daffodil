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

package org.apache.daffodil.section13.decimal

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestDecimalSigned extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/decimal/TestDecimalSigned.tdml"
}

class TestDecimalSigned extends TdmlTests {
  val tdmlSuite = TestDecimalSigned

  @Test def parseTestDecimalSigned_no_binary = test
  @Test def parseTestdecimalSigned_no_bcd = test
  @Test def unparseTestDecimalSigned_no_binary = test
  @Test def unparseTestdecimalSigned_no_bcd = test
  @Test def parseTestDecimalSigned_no_packed = test
  @Test def unparseTestDecimalSigned_no_packed = test
  @Test def parseTestDecimalSigned_no_ibm4690Packed = test
  @Test def unparseTestDecimalSigned_no_ibm4690Packed = test

  @Test def parseTestDecimalSigned_no_packed_delimited = test
  @Test def unparseTestDecimalSigned_no_packed_delimited = test
}
