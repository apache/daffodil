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

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

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
  @Test def test_DFDL_2399(): Unit = { runner.runOneTest("test_DFDL_2399") }

  // DAFFODIL-2378 (decided as not a bug. These tests characterize that behavior.)
  @Test def testTextNumberPattern1(): Unit = { runner.runOneTest("textNumberPattern1") }
  @Test def testTextNumberPattern2(): Unit = { runner.runOneTest("textNumberPattern2") }
  @Test def testTextNumberPattern3(): Unit = { runner.runOneTest("textNumberPattern3") }


  @Test def test_nameDOB_test2_pass(): Unit = { runner2.runOneTest("nameDOB_test2_pass") }
  @Test def test_nameDOB_test2_fail(): Unit = { runner2.runOneTest("nameDOB_test2_fail") }

  /*//DFDL-1118
  @Test def test_dfdl_782() = {
    val tr = new CustomTraceRunner
    tr.init
    val crunner = new CustomInteractiveDebuggerRunner(tr)
    val db = new InteractiveDebugger(crunner, ExpressionCompiler)
    Debugger.setDebugging(true)
    Debugger.setDebugger(db)

    runner.runOneTest("test_DFDL_782")

    // Comment out these two lines to see issue
    // documented in DFDL-790
    Debugger.setDebugging(false)
    Debugger.setDebugger(null)
  }
*/

}

/*
class CustomInteractiveDebuggerRunner(dr: DebuggerRunner)
  extends InteractiveDebuggerRunner {
  def init(id: InteractiveDebugger): Unit = dr.init
  def getCommand(): String = dr.getCommand
  def lineOutput(line: String): Unit = dr.lineOutput(line)
  def fini(): Unit = dr.fini
}

class CustomTraceRunner extends TraceRunner {
  private var _lines = List.empty[String]

  def getAllTheLines(): String = {
    val sb = new StringBuilder
    _lines.foreach(line => {
      if (line.length > 0) sb.append(line)
    })
    val allTheLines = sb.toString
    allTheLines
  }

  override def init(): Unit = { _lines = List.empty[String] }
  override def lineOutput(line: String) = _lines ++ (line + "\n")

}
*/
