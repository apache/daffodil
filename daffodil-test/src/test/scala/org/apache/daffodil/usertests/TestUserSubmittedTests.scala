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
import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.runtime1.debugger.InteractiveDebugger
import org.apache.daffodil.runtime1.debugger.TraceDebuggerRunner

import org.junit.Assert.assertTrue
import org.junit.Test

object TestUserSubmittedTests extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/usertests/UserSubmittedTests.tdml"
}

object TestNameDOB extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/usertests/nameDOB_test.tdml"
}

class TestUserSubmittedTests extends TdmlTests {
  val tdmlSuite = TestUserSubmittedTests

  @Test def test_prefix_separator_as_variable = test
  @Test def test_DFDL_2262 = test
  @Test def test_DFDL_2586 = test
  @Test def test_DFDL_2399 = test

  // DAFFODIL-2378 (decided as not a bug. These tests characterize that behavior.)
  @Test def textNumberPattern1 = test
  @Test def textNumberPattern2 = test
  @Test def textNumberPattern3 = test

  // custom trace debugger runner that just counts the number of lines output by the debugger
  class CountTraceDebuggerRunner extends TraceDebuggerRunner {
    var numLines = 0
    override def lineOutput(line: String) = numLines += 1
  }

  @Test def test_DFDL_782() = {
    val crunner = new CountTraceDebuggerRunner
    val db = new InteractiveDebugger(crunner, ExpressionCompilers)

    // sets the debugger and enables debugging
    tdmlSuite.runner.setDebugger(db)

    // run a test with the debugger and debugging enabled so that we count the lines. Running
    // the test will disable debugging when it completes
    test
    assertTrue(crunner.numLines > 0)

    // reset the numLines counter to 0
    crunner.numLines = 0

    // run the test again, this should not count any lines because debugging was disabled when
    // the previous test finished
    test
    assertTrue(crunner.numLines == 0)
  }
}

class TestNameDOB extends TdmlTests {
  val tdmlSuite = TestNameDOB

  @Test def nameDOB_test2_pass = test
}
