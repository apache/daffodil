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

package org.apache.daffodil.usertests

import org.apache.daffodil.core.dsom.ExpressionCompilers
import org.apache.daffodil.runtime1.debugger.InteractiveDebugger
import org.apache.daffodil.runtime1.debugger.TraceDebuggerRunner
import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Assert._
import org.junit.Test

object TestUserSubmittedTests {
  val testDir = "/org/apache/daffodil/usertests/"
  val runner = Runner(testDir, "UserSubmittedTests.tdml")
  val runner2 = Runner(testDir, "nameDOB_test.tdml")
  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner2.reset
  }

}

class TestUserSubmittedTests {

  import TestUserSubmittedTests._

  @Test def test_prefix_separator_as_variable(): Unit = {
    runner.runOneTest("test_prefix_separator_as_variable")
  }
  @Test def test_DFDL_2262(): Unit = { runner.runOneTest("test_DFDL_2262") }
  @Test def test_DFDL_2586(): Unit = { runner.runOneTest("test_DFDL_2586") }
  @Test def test_DFDL_2399(): Unit = { runner.runOneTest("test_DFDL_2399") }

  // DAFFODIL-2378 (decided as not a bug. These tests characterize that behavior.)
  @Test def testTextNumberPattern1(): Unit = { runner.runOneTest("textNumberPattern1") }
  @Test def testTextNumberPattern2(): Unit = { runner.runOneTest("textNumberPattern2") }
  @Test def testTextNumberPattern3(): Unit = { runner.runOneTest("textNumberPattern3") }

  @Test def test_nameDOB_test2_pass(): Unit = { runner2.runOneTest("nameDOB_test2_pass") }
  @Test def test_nameDOB_test2_fail(): Unit = { runner2.runOneTest("nameDOB_test2_fail") }

  @Test def test_dfdl_782() = {
    val crunner = new CountTraceDebuggerRunner
    val db = new InteractiveDebugger(crunner, ExpressionCompilers)

    // sets the debugger and enables debugging
    runner.setDebugger(db)

    // run a test with the debugger and debugging enabled so that we count the lines. runOneTest
    // will disable debugging when the test completes
    runner.runOneTest("test_DFDL_782")
    assertTrue(crunner.numLines > 0)

    // reset the numLines counter to 0
    crunner.numLines = 0

    // run the test again, this should not count any lines because debugging was disabled when
    // the previous call to runOneTest finished
    runner.runOneTest("test_DFDL_782")
    assertTrue(crunner.numLines == 0)

    // note that this Runner still has the CountTraceDebuggerRunner set as its debugger, so if
    // other tests using the same Runner enable debugging via runner.debug and run after this
    // test, they will use it and it might affect their behavior. Technically this isn't
    // necessary since no other tests in this suite do this, but it's good habit. But commenting
    // this line out should not break anything.
    runner.setDebugger(null)
  }

}

// custom trace debugger runner that just counts the number of lines output by the debugger
class CountTraceDebuggerRunner extends TraceDebuggerRunner {
  var numLines = 0
  override def lineOutput(line: String) = numLines += 1
}
