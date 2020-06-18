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

package org.apache.daffodil.section05.simple_types

import org.apache.daffodil.tdml.Runner
import org.junit._

object TestSimpleTypesUnparse {

  val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "SimpleTypesUnparse.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestSimpleTypesUnparse {
  import TestSimpleTypesUnparse._
 
  //DFDL-1454
  //dfdl:hexBinary not behaving like xs:hexBinary 
  //@Test def test_hexBinary_unparse_13() { runner.runOneTest("hexBinary_unparse_13") }
  
  @Test def test_hexBinary_unparse_01(): Unit = { runner.runOneTest("hexBinary_unparse_01") }
  @Test def test_hexBinary_unparse_02(): Unit = { runner.runOneTest("hexBinary_unparse_02") }
  @Test def test_hexBinary_unparse_03(): Unit = { runner.runOneTest("hexBinary_unparse_03") }
  @Test def test_hexBinary_unparse_04(): Unit = { runner.runOneTest("hexBinary_unparse_04") }
  @Test def test_hexBinary_unparse_05(): Unit = { runner.runOneTest("hexBinary_unparse_05") }
  @Test def test_hexBinary_unparse_06(): Unit = { runner.runOneTest("hexBinary_unparse_06") }
  @Test def test_hexBinary_unparse_07(): Unit = { runner.runOneTest("hexBinary_unparse_07") }
  @Test def test_hexBinary_unparse_08(): Unit = { runner.runOneTest("hexBinary_unparse_08") }
  @Test def test_hexBinary_unparse_09(): Unit = { runner.runOneTest("hexBinary_unparse_09") }
  @Test def test_hexBinary_unparse_10(): Unit = { runner.runOneTest("hexBinary_unparse_10") }
  @Test def test_hexBinary_unparse_11(): Unit = { runner.runOneTest("hexBinary_unparse_11") }
  @Test def test_hexBinary_unparse_12(): Unit = { runner.runOneTest("hexBinary_unparse_12") }
  @Test def test_hexBinary_unparse_14(): Unit = { runner.runOneTest("hexBinary_unparse_14") }
  @Test def test_hexBinary_unparse_15(): Unit = { runner.runOneTest("hexBinary_unparse_15") }
  @Test def test_hexBinary_unparse_16(): Unit = { runner.runOneTest("hexBinary_unparse_16") }
  @Test def test_hexBinary_unparse_17(): Unit = { runner.runOneTest("hexBinary_unparse_17") }
  @Test def test_hexBinary_unparse_18(): Unit = { runner.runOneTest("hexBinary_unparse_18") }
  @Test def test_hexBinary_unparse_19(): Unit = { runner.runOneTest("hexBinary_unparse_19") }

  @Test def test_hexBinary_variable_unparse_01(): Unit = { runner.runOneTest("hexBinary_variable_unparse_01") }
  @Test def test_hexBinary_variable_unparse_02(): Unit = { runner.runOneTest("hexBinary_variable_unparse_02") }
  @Test def test_hexBinary_variable_unparse_03(): Unit = { runner.runOneTest("hexBinary_variable_unparse_03") }
  @Test def test_hexBinary_variable_unparse_04(): Unit = { runner.runOneTest("hexBinary_variable_unparse_04") }

  @Test def test_float_binary_unparse_01(): Unit = { runner.runOneTest("float_binary_unparse_01") }
  @Test def test_double_binary_unparse_01(): Unit = { runner.runOneTest("double_binary_unparse_01") }
  @Test def test_integer_binary_unparse_01(): Unit = { runner.runOneTest("integer_binary_unparse_01") }
}
