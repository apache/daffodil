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

package org.apache.daffodil

import org.apache.daffodil.tdml.Runner
import org.junit.{ AfterClass, Test }

object TresysTests3 {
  val testDir = "/test-suite/tresys-contributed/"

  val runnerBF = Runner(testDir, "bitFlagExpression.tdml",
    compileAllTopLevel = false) // test has elements that have upward paths past root.

  val runnerAH = Runner(testDir, "AH.tdml", compileAllTopLevel = true)

  val runnerAM = Runner(testDir, "AM.tdml", validateTDMLFile = true, validateDFDLSchemas = false,
    compileAllTopLevel = true)

  val runnerAU = Runner(testDir, "AU.tdml", validateTDMLFile = true, validateDFDLSchemas = false,
    compileAllTopLevel = true)

  val runnerBC = Runner(testDir, "BC.tdml")
  val runnerBD = Runner(testDir, "BD.tdml")

  @AfterClass def shutDown(): Unit = {
    runnerBF.reset
    runnerAH.reset
    runnerAM.reset
    runnerAU.reset
    runnerBC.reset
    runnerBD.reset
  }
}

class TresysTests3 {
  import TresysTests3._

  @Test def test_testNone() = { runnerBF.runOneTest("testNone") }
  @Test def test_testOne(): Unit = { runnerBF.runOneTest("testOne") }
  @Test def test_testMany(): Unit = { runnerBF.runOneTest("testMany") }

  @Test def test_AH000(): Unit = { runnerAH.runOneTest("AH000") }
  @Test def test_AH001(): Unit = { runnerAH.runOneTest("AH001") }
  @Test def test_AH002(): Unit = { runnerAH.runOneTest("AH002") }

  // AM is a MIME style example
  // Wasn't working for lack of occursCountKind, and
  // because the bytes were flipped. It was written assuming that
  // Hex like A1B2 was interpreted as little endian words. I.e, the first
  // byte in that would be B2.
  // That's not how TDML works anyway. A1 is first. So by swizzling the indexes
  // the tests were asking for. Voila, they work.
  //
  // NOTE: AM.dfdl.xsd isn't a valid schema because it has an array in a hidden
  // group. Because everything inside a hidden group must be either default or
  // OVC, and arrays can't have either, they cannot be in hidden groups.
  // This is fixed by specifying daffodil-specific property
  // dfdlx:parseUnparsePolicy="parseOnly", which suppresses the check for this
  // constraint.

  @Test def test_AM000(): Unit = { runnerAM.runOneTest("AM000") }
  @Test def test_AM001(): Unit = { runnerAM.runOneTest("AM001") }

  @Test def test_AU000(): Unit = { runnerAU.runOneTest("AU000") } // packed and bcd

}
