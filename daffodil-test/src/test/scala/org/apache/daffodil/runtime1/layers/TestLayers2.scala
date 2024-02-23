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

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
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
  // In the wrap methods, a processing error is escalated to an RSDE.
  @Test def testBombWrapInputProcErr(): Unit = runnerB.runOneTest("testBombWrapInputProcErr")
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
  //
  // Throwing from each place in the API
  //
  // In the setter, getter, read and close methods, a throw of an exception
  // behaves the same as a processingError() call, i.e., causes backtracking.
  //
  @Test def testBombSetterThrowEX(): Unit = runnerB.runOneTest("testBombSetterThrowEX")
  @Test def testBombGetterThrowEX(): Unit = runnerB.runOneTest("testBombGetterThrowEX")
  @Test def testBombReadThrowEX(): Unit = runnerB.runOneTest("testBombReadThrowEX")
  @Test def testBombCloseInputThrowEX(): Unit = runnerB.runOneTest("testBombCloseInputThrowEX")
  // In the wrap methods, a processing error is escalated to an RSDE.
  @Test def testBombWrapInputThrowEX(): Unit = runnerB.runOneTest("testBombWrapInputThrowEX")
  @Test def testBombWrapOutputThrowEX(): Unit = runnerB.runOneTest("testBombWrapOutputThrowEX")
  //
  // In the write and close (output) methods, a processing error is an Unparse Error, which
  // terminates unparsing, but in the right way.
  //
  @Test def testBombWriteThrowEX(): Unit = runnerB.runOneTest("testBombWriteThrowEX")
  @Test def testBombWriteThrowEXWithSuspension(): Unit =
    runnerB.runOneTest("testBombWriteThrowEXWithSuspension")
  @Test def testBombCloseOutputThrowEX(): Unit =
    runnerB.runOneTest("testBombCloseOutputThrowEX")
  @Test def testBombCloseOutputThrowEXWithSuspension(): Unit =
    runnerB.runOneTest("testBombCloseOutputThrowEXWithSuspension")
}
