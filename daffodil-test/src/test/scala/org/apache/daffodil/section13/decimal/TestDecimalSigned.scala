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

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestDecimalSigned {
  val testDir = "/org/apache/daffodil/section13/decimal/"
  val runner: Runner = Runner(testDir, "TestDecimalSigned.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}
class TestDecimalSigned {
  import TestDecimalSigned._

  @Test def parseTestDecimalSigned_no_binary(): Unit =
    runner.runOneTest("parseTestDecimalSigned_no_binary")
  @Test def parseTestdecimalSigned_no_bcd(): Unit =
    runner.runOneTest("parseTestdecimalSigned_no_bcd")

// DAFFODIL-2957 - the tests below all failing
// Abort with usage error. Should be unparse error.
//  @Test def unparseTestDecimalSigned_no_binary(): Unit =
//    runner.runOneTest("unparseTestDecimalSigned_no_binary")
// error not detected
//  @Test def unparseTestdecimalSigned_no_bcd(): Unit =
//    runner.runOneTest("unparseTestdecimalSigned_no_bcd")
// error not detected
//  @Test def parseTestDecimalSigned_no_packed(): Unit =
//    runner.runOneTest("parseTestDecimalSigned_no_packed")
// error not detected
//  @Test def unparseTestDecimalSigned_no_packed(): Unit =
//    runner.runOneTest("unparseTestDecimalSigned_no_packed")
// error not detected
//  @Test def parseTestDecimalSigned_no_ibm4690Packed(): Unit =
//    runner.runOneTest("parseTestDecimalSigned_no_ibm4690Packed")
// error not detected
//  @Test def unparseTestDecimalSigned_no_ibm4690Packed(): Unit =
//    runner.runOneTest("unparseTestDecimalSigned_no_ibm4690Packed")
}
