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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestBitFlag extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/bitFlagExpression.tdml"
  override def createRunner() = Runner(tdmlDir, tdmlFile, compileAllTopLevel = false)
}

object TestAH extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AH.tdml"
  override def createRunner() = Runner(tdmlDir, tdmlFile, compileAllTopLevel = true)
}

object TestAM extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AM.tdml"
  override def createRunner() = Runner(
    tdmlDir,
    tdmlFile,
    validateTDMLFile = true,
    validateDFDLSchemas = false,
    compileAllTopLevel = true
  )
}

object TestAU extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AU.tdml"
  override def createRunner() = Runner(
    tdmlDir,
    tdmlFile,
    validateTDMLFile = true,
    validateDFDLSchemas = false,
    compileAllTopLevel = true
  )
}

class TestBitFlag extends TdmlTests {
  val tdmlSuite = TestBitFlag

  @Test def testNone = test
  @Test def testOne = test
  @Test def testMany = test
}

class TestAH extends TdmlTests {
  val tdmlSuite = TestAH

  @Test def AH000 = test
  @Test def AH001 = test
  @Test def AH002 = test
}

class TestAM extends TdmlTests {
  val tdmlSuite = TestAM

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
  @Test def AM000 = test
  @Test def AM001 = test
}

class TestAU extends TdmlTests {
  val tdmlSuite = TestAU

  @Test def AU000 = test
}
