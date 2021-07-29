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

import org.apache.daffodil.tdml.Runner
import org.junit._


object TestBitFunctions {
  val testDir = "/org/apache/daffodil/section23/dfdl_functions/"

  val runner = Runner(testDir, "BitFunctions.tdml")
  val runner1= Runner(testDir,"BitFunctionsXor.tdml")
  val runner2=Runner(testDir,"BitFunctionsOr.tdml")
  val runner3=Runner(testDir,"BitFunctionsAnd.tdml")
  val runner4=Runner(testDir,"BitFunctionsNot.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner1.reset
    runner2.reset
    runner3.reset
    runner4.reset
  }
}

class TestBitFunctions {
  import TestBitFunctions._
  @Test def longShiftTest():Unit = {runner.runOneTest("longShiftTest")}
  @Test def intShiftTest():Unit = {runner.runOneTest("intShiftTest")}
  @Test def byteShiftTest():Unit = {runner.runOneTest("byteShiftTest")}
  @Test def shortShiftTest():Unit = {runner.runOneTest("shortShiftTest")}
  @Test def testUnsignedLongShifting1():Unit = {runner.runOneTest("testUnsignedLongShifting1")}
  @Test def testUnsignedIntShifting():Unit = {runner.runOneTest("testUnsignedIntShifting")}
  @Test def testUnsignedByteShifting():Unit = {runner.runOneTest("testUnsignedByteShifting")}
  @Test def testUnsignedShortShifting():Unit = {runner.runOneTest("testUnsignedShortShifting")}
  @Test def leftShiftIntError():Unit = {runner.runOneTest("leftShiftIntError")}
  @Test def rightShiftIntError():Unit = {runner.runOneTest("rightShiftIntError")}
  @Test def rightShiftLongError():Unit = {runner.runOneTest("rightShiftLongError")}
  @Test def leftShiftLongError():Unit = {runner.runOneTest("leftShiftLongError")}
  @Test def leftShiftShortError():Unit = {runner.runOneTest("leftShiftShortError")}
  @Test def rightShiftShortError():Unit = {runner.runOneTest("rightShiftShortError")}
  @Test def leftShiftByteError():Unit = {runner.runOneTest("leftShiftByteError")}
  @Test def rightShiftByteError():Unit = {runner.runOneTest("rightShiftByteError")}
  @Test def leftShiftFloatError01():Unit = {runner.runOneTest("leftShiftFloatError01")}
  @Test def rightShiftFloatError01():Unit = {runner.runOneTest("rightShiftFloatError01")}
  @Test def leftShiftDoubleError01():Unit = {runner.runOneTest("leftShiftDoubleError01")}
  @Test def rightShiftDoubleError01():Unit = {runner.runOneTest("rightShiftDoubleError01")}
  @Test def leftShiftIntegerError01():Unit = {runner.runOneTest("leftShiftIntegerError01")}
  @Test def rightShiftIntegerError01():Unit = {runner.runOneTest("rightShiftIntegerError01")}
  @Test def leftShiftDecimalError01():Unit = {runner.runOneTest("leftShiftDecimalError01")}
  @Test def rightShiftDecimalError01():Unit = {runner.runOneTest("rightShiftDecimalError01")}
  @Test def leftShiftNonNegativeIntegerError01():Unit = {runner.runOneTest("leftShiftNonNegativeIntegerError01")}
  @Test def rightShiftNonNegativeIntegerError01():Unit = {runner.runOneTest("rightShiftNonNegativeIntegerError01")}
  @Test def testIntXor():Unit = {runner1.runOneTest("testIntXor")}
  @Test def testLongXor():Unit = {runner1.runOneTest("testLongXor")}
  @Test def testShortXor():Unit = {runner1.runOneTest("testShortXor")}
  @Test def testByteXor():Unit = {runner1.runOneTest("testByteXor")}
  @Test def testUnsignedIntXor():Unit = {runner1.runOneTest("testUnsignedIntXor")}
  @Test def testUnsignedLongXor():Unit = {runner1.runOneTest("testUnsignedLongXor")}
  @Test def testUnsignedShortXor():Unit = {runner1.runOneTest("testUnsignedShortXor")}
  @Test def testUnsignedByteXor():Unit = {runner1.runOneTest("testUnsignedByteXor")}
  @Test def testIntOr():Unit = {runner2.runOneTest("testIntOr")}
  @Test def testLongOr():Unit = {runner2.runOneTest("testLongOr")}
  @Test def testShortOr():Unit = {runner2.runOneTest("testShortOr")}
  @Test def testByteOr():Unit = {runner2.runOneTest("testByteOr")}
  @Test def testUnsignedIntOr():Unit = {runner2.runOneTest("testUnsignedIntOr")}
  @Test def testUnsignedLongOr():Unit = {runner2.runOneTest("testUnsignedLongOr")}
  @Test def testUnsignedShortOr():Unit = {runner2.runOneTest("testUnsignedShortOr")}
  @Test def testUnsignedByteOr():Unit = {runner2.runOneTest("testUnsignedByteOr")}
  @Test def testIntAnd():Unit = {runner3.runOneTest("testIntAnd")}
  @Test def testLongAnd():Unit = {runner3.runOneTest("testLongAnd")}
  @Test def testShortAnd():Unit = {runner3.runOneTest("testShortAnd")}
  @Test def testByteAnd():Unit = {runner3.runOneTest("testByteAnd")}
  @Test def testUnsignedIntAnd():Unit = {runner3.runOneTest("testUnsignedIntAnd")}
  @Test def testUnsignedLongAnd():Unit = {runner3.runOneTest("testUnsignedLongAnd")}
  @Test def testUnsignedShortAnd():Unit = {runner3.runOneTest("testUnsignedShortAnd")}
  @Test def testUnsignedByteAnd():Unit = {runner3.runOneTest("testUnsignedByteAnd")}
  @Test def testIntNot():Unit = {runner4.runOneTest("testIntNot")}
  @Test def testLongNot():Unit = {runner4.runOneTest("testLongNot")}
  @Test def testShortNot():Unit = {runner4.runOneTest("testShortNot")}
  @Test def testByteNot():Unit = {runner4.runOneTest("testByteNot")}
  @Test def testUnsignedIntNot():Unit = {runner4.runOneTest("testUnsignedIntNot")}
  @Test def testUnsignedLongNot():Unit = {runner4.runOneTest("testUnsignedLongNot")}
  @Test def testUnsignedShortNot():Unit = {runner4.runOneTest("testUnsignedShortNot")}
  @Test def testUnsignedByteNot():Unit = {runner4.runOneTest("testUnsignedByteNot")}
}
