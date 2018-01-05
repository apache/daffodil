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
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.debugger.InteractiveDebugger
import org.apache.daffodil.debugger.TraceDebuggerRunner
import org.apache.daffodil.dsom.ExpressionCompilers

class TestUserSubmittedTestsDebug {
  val testDir = "/org/apache/daffodil/usertests/"
  val aa = testDir + "UserSubmittedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  //DFDL-1118
  @Test def test_dfdl_782() = {
    val tr = new CustomTraceRunner1
    val db = new InteractiveDebugger(tr, ExpressionCompilers)
    runner.setDebugger(db)
    runner.setDebugging(true)

    runner.runOneTest("test_DFDL_782")

    // Comment out this line to see issue
    // documented in DFDL-790
    runner.setDebugging(false)
  }

}

class CustomTraceRunner1 extends TraceDebuggerRunner {
  private var _lines = List.empty[String]

  def getAllTheLines(): String = {
    val sb = new StringBuilder
    _lines.foreach(line => {
      if (line.length > 0) sb.append(line)
    })
    val allTheLines = sb.toString
    allTheLines
  }

  override def init(id: InteractiveDebugger): Unit = {
    _lines = List.empty[String]
    super.init(id)
  }
  override def lineOutput(line: String) = _lines :+ (line + "\n")

}
