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

package org.apache.daffodil.runtime1.layers

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.exceptions.Abort
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.TDMLException

import org.junit.AfterClass
import org.junit.Assert.fail
import org.junit.Test

object TestLayers2 {

  val testDir = "/org/apache/daffodil/layers/"
  val runnerB = Runner(testDir, "TestLayersBomb.tdml")

  @AfterClass def shutDown(): Unit = {
    runnerB.reset()
  }
}

class TestLayers2 {

  import TestLayers2._

  //
  // Properly configured, detected, and loaded layer, now what happens at runtime if
  // it Bombs out in some location by calling processingError(), runtimeSDE(), or throwing
  // something.
  //
  // These should never result in an abort, no matter what the layer does.
  // with the exception of explicitly calling Assert.abort (or equivalent thereof.)
  //
  @Test def testNoBomb(): Unit = runnerB.runOneTest("testNoBomb") // baseline
  @Test def testNoBomb2(): Unit = runnerB.runOneTest("testNoBomb2") // baseline
  @Test def testNoBomb3(): Unit = runnerB.runOneTest("testNoBomb3") // baseline

  //
  // Runtime SDE from each place in the API
  //
  @Test def testBombSetterRSDE(): Unit = runnerB.runOneTest("testBombSetterRSDE")
  @Test def testBombGetterRSDE(): Unit = runnerB.runOneTest("testBombGetterRSDE")
  @Test def testBombCloseOutputRSDE(): Unit = runnerB.runOneTest("testBombCloseOutputRSDE")
  @Test def testBombCloseOutputRSDEWithSuspension(): Unit =
    runnerB.runOneTest("testBombCloseOutputRSDEWithSuspension")
  @Test def testBombWriteRSDE(): Unit = runnerB.runOneTest("testBombWriteRSDE")
  @Test def testBombWriteRSDEWithSuspension(): Unit =
    runnerB.runOneTest("testBombWriteRSDEWithSuspension")
  @Test def testBombReadRSDE(): Unit = runnerB.runOneTest("testBombReadRSDE")
  @Test def testBombCloseInputRSDE(): Unit = runnerB.runOneTest("testBombCloseInputRSDE")
  @Test def testBombWrapInputRSDE(): Unit = runnerB.runOneTest("testBombWrapInputRSDE")
  @Test def testBombWrapOutputRSDE(): Unit = runnerB.runOneTest("testBombWrapOutputRSDE")
  //
  // Processing Error from each place in the API
  //
  // In the setter, getter, read and close methods, a processing error causes
  // backtracking.
  //
  @Test def testBombSetterProcErr(): Unit = runnerB.runOneTest("testBombSetterProcErr")
  @Test def testBombGetterProcErr(): Unit = runnerB.runOneTest("testBombGetterProcErr")
  @Test def testBombReadProcErr(): Unit = runnerB.runOneTest("testBombReadProcErr")
  @Test def testBombCloseInputProcErr(): Unit = runnerB.runOneTest("testBombCloseInputProcErr")
  @Test def testBombWrapInputProcErr(): Unit = runnerB.runOneTest("testBombWrapInputProcErr")
  @Test def testBombWrapInputProcErrWithCause(): Unit =
    runnerB.runOneTest("testBombWrapInputProcErrWithCause")
  @Test def testBombWrapOutputProcErr(): Unit = runnerB.runOneTest("testBombWrapOutputProcErr")
  //
  // In the write and close (output) methods, a processing error is an Unparse Error, which
  // terminates unparsing, but in the right way.
  //
  @Test def testBombWriteProcErr(): Unit = runnerB.runOneTest("testBombWriteProcErr")
  @Test def testBombWriteProcErrWithSuspension(): Unit =
    runnerB.runOneTest("testBombWriteProcErrWithSuspension")
  @Test def testBombCloseOutputProcErr(): Unit =
    runnerB.runOneTest("testBombCloseOutputProcErr")
  @Test def testBombCloseOutputProcErrWithSuspension(): Unit =
    runnerB.runOneTest("testBombCloseOutputProcErrWithSuspension")
  @Test def testBombGetterProcErrWithSuspension(): Unit =
    runnerB.runOneTest("testBombGetterProcErrWithSuspension")

  //
  // Throwing from each place in the API
  //

  @Test def testBombSetterThrowEX(): Unit = runnerB.runOneTest("testBombSetterThrowEX")
  @Test def testBombGetterThrowEX(): Unit = runnerB.runOneTest("testBombGetterThrowEX")
  @Test def testBombReadThrowEX(): Unit = runnerB.runOneTest("testBombReadThrowEX")
  @Test def testBombCloseInputThrowEX(): Unit = runnerB.runOneTest("testBombCloseInputThrowEX")
  @Test def testBombWrapInputThrowEX(): Unit = runnerB.runOneTest("testBombWrapInputThrowEX")
  @Test def testBombWrapOutputThrowEX(): Unit = runnerB.runOneTest("testBombWrapOutputThrowEX")
  @Test def testBombWriteThrowEX(): Unit = runnerB.runOneTest("testBombWriteThrowEX")
  @Test def testBombWriteThrowEXWithSuspension(): Unit =
    runnerB.runOneTest("testBombWriteThrowEXWithSuspension")
  @Test def testBombCloseOutputThrowEX(): Unit =
    runnerB.runOneTest("testBombCloseOutputThrowEX")
  @Test def testBombCloseOutputThrowEXWithSuspension(): Unit =
    runnerB.runOneTest("testBombCloseOutputThrowEXWithSuspension")

  //
  // KEEP THIS STUFF - These are the tests to use if the bomb layer test rig does NOT
  // convert all Exceptions to PEs. We might want to test it both ways.
  //
//  def handleEX(testName: String) = {
//    val h = new Handle("ThrowEX") {
//      def f(s: String) =
//        intercept[Exception] {
//          runnerB.runOneTest(testName)
//        }.asInstanceOf[Exception]
//    }
//    h(testName)
//  }
//  @Test def testBombSetterThrowEX(): Unit = handleEX("testBombSetterThrowEX")
//  @Test def testBombGetterThrowEX(): Unit = handleEX("testBombGetterThrowEX")
//  @Test def testBombReadThrowEX(): Unit = handleEX("testBombReadThrowEX")
//  @Test def testBombCloseInputThrowEX(): Unit = handleEX("testBombCloseInputThrowEX")
//  @Test def testBombWrapInputThrowEX(): Unit = handleEX("testBombWrapInputThrowEX")
//  @Test def testBombWrapOutputThrowEX(): Unit = handleEX("testBombWrapOutputThrowEX")
//  @Test def testBombWriteThrowEX(): Unit = handleEX("testBombWriteThrowEX")
//  @Test def testBombWriteThrowEXWithSuspension(): Unit =
//    handleEX("testBombWriteThrowEXWithSuspension")
//  @Test def testBombCloseOutputThrowEX(): Unit =
//    handleEX("testBombCloseOutputThrowEX")
//  @Test def testBombCloseOutputThrowEXWithSuspension(): Unit =
//    handleEX("testBombCloseOutputThrowEXWithSuspension")

  //
  // Runtime Exception thrown from each place in the API
  //
  abstract class Handle(kind: String) {
    def apply(testName: String) = {
      val contains = testName
        .replace("testBomb", "")
        .replace(kind, "")
        .replace("WithSuspension", "")
        .toLowerCase
      val e = f(testName)
      val cMsg = TestUtils.getAMessage(e)
      // println(s">>>>\n     $cMsg\n<<<<<") // keep this.
      if (cMsg == null)
        fail("no cause message")
      if (!cMsg.toLowerCase.contains(contains))
        fail(s"'$cMsg' does not contain '$contains'.")
    }

    def f(s: String): Throwable
  }

  def handle(testName: String) = {
    val h = new Handle("ThrowRE") {
      def f(s: String) =
        intercept[TDMLException] {
          runnerB.runOneTest(testName)
        }.asInstanceOf[Exception]
    }
    h(testName)
  }

  @Test def testBombSetterThrowRE(): Unit = handle("testBombSetterThrowRE")
  @Test def testBombGetterThrowRE(): Unit = handle("testBombGetterThrowRE")
  @Test def testBombCloseOutputThrowRE(): Unit = handle("testBombCloseOutputThrowRE")
  @Test def testBombCloseOutputThrowREWithSuspension(): Unit = handle(
    "testBombCloseOutputThrowREWithSuspension",
  )
  @Test def testBombWriteThrowRE(): Unit = handle("testBombWriteThrowRE")
  @Test def testBombWriteThrowREWithSuspension(): Unit = handle(
    "testBombWriteThrowREWithSuspension",
  )
  @Test def testBombReadThrowRE(): Unit = handle("testBombReadThrowRE")
  @Test def testBombCloseInputThrowRE(): Unit = handle("testBombCloseInputThrowRE")
  @Test def testBombWrapInputThrowRE(): Unit = handle("testBombWrapInputThrowRE")
  @Test def testBombWrapOutputThrowRE(): Unit = handle("testBombWrapOutputThrowRE")

  def handleA(testName: String): Unit = {
    val h = new Handle("Abort") {
      def f(s: String) =
        intercept[Abort] {
          runnerB.runOneTest(testName)
        }
    }
    h(testName)
  }

  @Test def testBombSetterAbort(): Unit = handleA("testBombSetterAbort")
  @Test def testBombGetterAbort(): Unit = handleA("testBombGetterAbort")
  @Test def testBombCloseOutputAbort(): Unit = handleA("testBombCloseOutputAbort")
  @Test def testBombCloseOutputAbortWithSuspension(): Unit = handleA(
    "testBombCloseOutputAbortWithSuspension",
  )
  @Test def testBombWriteAbort(): Unit = handleA("testBombWriteAbort")
  @Test def testBombWriteAbortWithSuspension(): Unit = handleA(
    "testBombWriteAbortWithSuspension",
  )
  @Test def testBombReadAbort(): Unit = handleA("testBombReadAbort")
  @Test def testBombCloseInputAbort(): Unit = handleA("testBombCloseInputAbort")
  @Test def testBombWrapInputAbort(): Unit = handleA("testBombWrapInputAbort")
  @Test def testBombWrapOutputAbort(): Unit = handleA("testBombWrapOutputAbort")

}
