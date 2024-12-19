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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.exceptions.Abort
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.tdml.TDMLException

import org.junit.Assert.fail
import org.junit.Test

object TestLayers2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/layers/TestLayersBomb.tdml"
}

class TestLayers2 extends TdmlTests {
  val tdmlSuite = TestLayers2

  //
  // Properly configured, detected, and loaded layer, now what happens at runtime if
  // it Bombs out in some location by calling processingError(), runtimeSDE(), or throwing
  // something.
  //
  // These should never result in an abort, no matter what the layer does.
  // with the exception of explicitly calling Assert.abort (or equivalent thereof.)
  //
  @Test def testNoBomb = test // baseline
  @Test def testNoBomb2 = test // baseline
  @Test def testNoBomb3 = test // baseline

  //
  // Runtime SDE from each place in the API
  //
  @Test def testBombSetterRSDE = test
  @Test def testBombGetterRSDE = test
  @Test def testBombCloseOutputRSDE = test
  @Test def testBombCloseOutputRSDEWithSuspension = test
  @Test def testBombWriteRSDE = test
  @Test def testBombWriteRSDEWithSuspension = test
  @Test def testBombReadRSDE = test
  @Test def testBombCloseInputRSDE = test
  @Test def testBombWrapInputRSDE = test
  @Test def testBombWrapOutputRSDE = test
  //
  // Processing Error from each place in the API
  //
  // In the setter, getter, read and close methods, a processing error causes
  // backtracking.
  //
  @Test def testBombSetterProcErr = test
  @Test def testBombGetterProcErr = test
  @Test def testBombReadProcErr = test
  @Test def testBombCloseInputProcErr = test
  @Test def testBombWrapInputProcErr = test
  @Test def testBombWrapInputProcErrWithCause = test
  @Test def testBombWrapOutputProcErr = test
  //
  // In the write and close (output) methods, a processing error is an Unparse Error, which
  // terminates unparsing, but in the right way.
  //
  @Test def testBombWriteProcErr = test
  @Test def testBombWriteProcErrWithSuspension = test
  @Test def testBombCloseOutputProcErr = test
  @Test def testBombCloseOutputProcErrWithSuspension = test
  @Test def testBombGetterProcErrWithSuspension = test

  // This function is useful if the bomb layer test rig does NOT convert all Exceptions to PEs.
  // We might want to test it both ways. See below tests.
  def handleEX = {
    val h = new Handle("ThrowEX") {
      def f() =
        intercept[Exception] {
          test
        }.asInstanceOf[Exception]
    }
    h()
  }

  //
  // Throwing from each place in the API
  //
  // If the bomb layer test rig does not convert all Exceptions to PE, it might be helpful to
  // change the below "test" calls to "handleEX"
  @Test def testBombSetterThrowEX = test
  @Test def testBombGetterThrowEX = test
  @Test def testBombReadThrowEX = test
  @Test def testBombCloseInputThrowEX = test
  @Test def testBombWrapInputThrowEX = test
  @Test def testBombWrapOutputThrowEX = test
  @Test def testBombWriteThrowEX = test
  @Test def testBombWriteThrowEXWithSuspension = test
  @Test def testBombCloseOutputThrowEX = test
  @Test def testBombCloseOutputThrowEXWithSuspension = test

  //
  // Runtime Exception thrown from each place in the API
  //
  abstract class Handle(kind: String) {
    def apply() = {
      val contains = testName
        .replace("testBomb", "")
        .replace(kind, "")
        .replace("WithSuspension", "")
        .toLowerCase
      val e = f()
      val cMsg = Misc.getAMessage(e)
      // println(s">>>>\n     $cMsg\n<<<<<") // keep this.
      if (cMsg == null)
        fail("no cause message")
      if (!cMsg.toLowerCase.contains(contains))
        fail(s"'$cMsg' does not contain '$contains'.")
    }

    def f(): Throwable
  }

  def handle = {
    val h = new Handle("ThrowRE") {
      def f() =
        intercept[TDMLException] {
          test
        }.asInstanceOf[Exception]
    }
    h()
  }

  @Test def testBombSetterThrowRE = handle
  @Test def testBombGetterThrowRE = handle
  @Test def testBombCloseOutputThrowRE = handle
  @Test def testBombCloseOutputThrowREWithSuspension = handle
  @Test def testBombWriteThrowRE = handle
  @Test def testBombWriteThrowREWithSuspension = handle
  @Test def testBombReadThrowRE = handle
  @Test def testBombCloseInputThrowRE = handle
  @Test def testBombWrapInputThrowRE = handle
  @Test def testBombWrapOutputThrowRE = handle

  def handleA: Unit = {
    val h = new Handle("Abort") {
      def f() =
        intercept[Abort] {
          test
        }
    }
    h()
  }

  @Test def testBombSetterAbort = handleA
  @Test def testBombGetterAbort = handleA
  @Test def testBombCloseOutputAbort = handleA
  @Test def testBombCloseOutputAbortWithSuspension = handleA
  @Test def testBombWriteAbort = handleA
  @Test def testBombWriteAbortWithSuspension = handleA
  @Test def testBombReadAbort = handleA
  @Test def testBombCloseInputAbort = handleA
  @Test def testBombWrapInputAbort = handleA
  @Test def testBombWrapOutputAbort = handleA
}
