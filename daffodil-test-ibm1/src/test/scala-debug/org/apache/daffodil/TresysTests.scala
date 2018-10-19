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

import org.junit.Test
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.util.Misc

class TresysTestsDebug {

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name")
  // }

  val testDir = "/test-suite/tresys-contributed/"

  // Jira DFDL-1392 - Issue with escapeEscape character that is first and precedes an escape-block start.
  // Is being removed, but should be preserved as it does not precede an escape character, nor an escape block end.
  val ba = testDir + "BA.tdml"
  lazy val runnerBA = new DFDLTestSuite(Misc.getRequiredResource(ba))
  @Test def test_BA000() { runnerBA.runOneTest("BA000") } // escape schemes and delimiters

}
