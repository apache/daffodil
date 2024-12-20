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

package org.apache.daffodil.section23.dfdl_expressions

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestBitFunctions extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/BitFunctions.tdml"
}

object TestBitFunctionsXor extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/BitFunctionsXor.tdml"
}

object TestBitFunctionsOr extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/BitFunctionsOr.tdml"
}

object TestBitFunctionsAnd extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/BitFunctionsAnd.tdml"
}

object TestBitFunctionsNot extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/BitFunctionsNot.tdml"
}

class TestBitFunctions extends TdmlTests {
  val tdmlSuite = TestBitFunctions

  @Test def longShiftTest = test
  @Test def intShiftTest = test
  @Test def byteShiftTest = test
  @Test def shortShiftTest = test
  @Test def testUnsignedLongShifting1 = test
  @Test def testUnsignedIntShifting = test
  @Test def testUnsignedByteShifting = test
  @Test def testUnsignedShortShifting = test
  @Test def leftShiftIntError = test
  @Test def rightShiftIntError = test
  @Test def rightShiftLongError = test
  @Test def leftShiftLongError = test
  @Test def leftShiftShortError = test
  @Test def rightShiftShortError = test
  @Test def leftShiftByteError = test
  @Test def rightShiftByteError = test
  @Test def leftShiftFloatError01 = test
  @Test def rightShiftFloatError01 = test
  @Test def leftShiftDoubleError01 = test
  @Test def rightShiftDoubleError01 = test
  @Test def leftShiftIntegerError01 = test
  @Test def rightShiftIntegerError01 = test
  @Test def leftShiftDecimalError01 = test
  @Test def rightShiftDecimalError01 = test
  @Test def leftShiftNonNegativeIntegerError01 = test
  @Test def rightShiftNonNegativeIntegerError01 = test
}

class TestBitFunctionsXor extends TdmlTests {
  val tdmlSuite = TestBitFunctionsXor

  @Test def testIntXor = test
  @Test def testLongXor = test
  @Test def testShortXor = test
  @Test def testByteXor = test
  @Test def testUnsignedIntXor = test
  @Test def testUnsignedLongXor = test
  @Test def testUnsignedShortXor = test
  @Test def testUnsignedByteXor = test
}

class TestBitFunctionsOr extends TdmlTests {
  val tdmlSuite = TestBitFunctionsOr

  @Test def testIntOr = test
  @Test def testLongOr = test
  @Test def testShortOr = test
  @Test def testByteOr = test
  @Test def testUnsignedIntOr = test
  @Test def testUnsignedLongOr = test
  @Test def testUnsignedShortOr = test
  @Test def testUnsignedByteOr = test
}

class TestBitFunctionsAnd extends TdmlTests {
  val tdmlSuite = TestBitFunctionsAnd

  @Test def testIntAnd = test
  @Test def testLongAnd = test
  @Test def testShortAnd = test
  @Test def testByteAnd = test
  @Test def testUnsignedIntAnd = test
  @Test def testUnsignedLongAnd = test
  @Test def testUnsignedShortAnd = test
  @Test def testUnsignedByteAnd = test
}

class TestBitFunctionsNot extends TdmlTests {
  val tdmlSuite = TestBitFunctionsNot

  @Test def testIntNot = test
  @Test def testLongNot = test
  @Test def testShortNot = test
  @Test def testByteNot = test
  @Test def testUnsignedIntNot = test
  @Test def testUnsignedLongNot = test
  @Test def testUnsignedShortNot = test
  @Test def testUnsignedByteNot = test
}
