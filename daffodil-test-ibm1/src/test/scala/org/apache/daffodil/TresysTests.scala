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

import org.junit.{ AfterClass, Test }
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.RunnerOpts

object TresysTests {

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name")
  // }

  val testDir = "/test-suite/tresys-contributed/"

  lazy val runnerDelimited = Runner(testDir, "delimTests.tdml")

  lazy val runnerMD = Runner(testDir, "multiple-diagnostics.tdml", RunnerOpts(compileAllTopLevel = true))
  lazy val runnerMD_NV = Runner(testDir, "multiple-diagnostics.tdml", RunnerOpts(compileAllTopLevel = true, validateDFDLSchemas = false))
  
  lazy val runnerBB = Runner(testDir, "BB.tdml")
  lazy val runnerBE = Runner(testDir, "BE.tdml")
  lazy val runnerBF1 = Runner(testDir, "BF.tdml")
  lazy val runnerBG = Runner(testDir, "BG.tdml")
  lazy val runnerMB = Runner(testDir, "mixed-binary-text.tdml")

  lazy val runnerAP = Runner(testDir, "AP.tdml")
  lazy val runnerAX = Runner(testDir, "AX.tdml")
  lazy val runnerAV000 = Runner(testDir, "AV000.tdml")
  lazy val runnerAV001 = Runner(testDir, "AV001.tdml")
  lazy val runnerAV002 = Runner(testDir, "AV002.tdml")
  lazy val runnerAV003 = Runner(testDir, "AV003.tdml")
  lazy val runnerNSD = Runner(testDir, "nested-separator-delimited.tdml")

  lazy val runnerRD = Runner(testDir, "runtime-diagnostics.tdml", RunnerOpts(compileAllTopLevel = true, validateTDMLFile = false))

  lazy val runnerSQ = Runner(testDir, "sequence.tdml")

  lazy val runnerNG = Runner(testDir, "nested_group_ref.tdml")

  lazy val runnerAF = Runner(testDir, "AF.tdml")

  lazy val runnerAG = Runner(testDir, "AG.tdml")

  lazy val runnerAW = Runner(testDir, "AW.tdml")

  lazy val runnerAY = Runner(testDir, "AY.tdml")

  lazy val runnerAZ = Runner(testDir, "AZ.tdml")

  lazy val runnerBA = Runner(testDir, "BA.tdml")

  lazy val runnerBC = Runner(testDir, "BC.tdml")

  lazy val runnerBD = Runner(testDir, "BD.tdml")

  @AfterClass def shutDown: Unit = {
    runnerDelimited.reset
    runnerMD.reset
    runnerMD_NV.reset
    runnerBB.reset
    runnerBE.reset
    runnerBF1.reset
    runnerBG.reset
    runnerMB.reset
    runnerAP.reset
    runnerAX.reset
    runnerAV000.reset
    runnerAV001.reset
    runnerAV002.reset
    runnerAV003.reset
    runnerNSD.reset
    runnerRD.reset
    runnerSQ.reset
    runnerNG.reset
    runnerAF.reset
    runnerAG.reset
    runnerAW.reset
    runnerAY.reset
    runnerAZ.reset
    runnerBA.reset
    runnerBC.reset
    runnerBD.reset
  }
}

class TresysTests {
  import TresysTests._

  @Test def test_length_delimited_12_03_controversial(): Unit = { runnerDelimited.runOneTest("length_delimited_12_03_controversial") }

  @Test def test_AX000() = { runnerAX.runOneTest("AX000") } // escape schemes

  @Test def test_AV000(): Unit = { runnerAV000.runOneTest("AV000") } // needs date

  @Test def test_AV001(): Unit = { runnerAV001.runOneTest("AV001") } // needs date

  @Test def test_AV002(): Unit = { runnerAV002.runOneTest("AV002") }

  @Test def test_AV003(): Unit = { runnerAV003.runOneTest("AV003") } // needs date

  @Test def test_multiple_diagnostics1(): Unit = { runnerMD.runOneTest("twoMissingTypeDefErrors") }
  @Test def test_multiple_diagnostics2(): Unit = { runnerMD.runOneTest("manyErrors1") }
  @Test def test_multiple_diagnostics3(): Unit = { runnerMD_NV.runOneTest("manyErrors2") }

  @Test def test_nested_separator_delimited_baseline(): Unit = { runnerNSD.runOneTest("baseline") }

  // Fails in IBM DFDL - ambiguous separator/terminator not accepted.
  @Test def test_nested_separator_delimited_baseline_ibm(): Unit = { runnerNSD.runOneTest("baseline_ibm") }

  @Test def test_nested_separator_delimited_basicNest(): Unit = { runnerNSD.runOneTest("basicNest") }
  @Test def test_nested_separator_delimited_basicNest2(): Unit = { runnerNSD.runOneTest("basicNest2")}

  // Nested delimiter issues - DAFFODIL-2101
  // @Test def test_nested_separator_delimited_nest1() { runnerNSD.runOneTest("nest1")}
  // Nested delimiter issues - DAFFODIL-2101
  // @Test def test_nested_separator_delimited_nest2() { runnerNSD.runOneTest("nest2")}
  @Test def test_nested_separator_delimited_nest3(): Unit = { runnerNSD.runOneTest("nest3")}

  @Test def test_runtime_diagnostics1(): Unit = { runnerRD.runOneTest("PE1") }

  @Test def test_seq1(): Unit = { runnerSQ.runOneTest("seq1") }

  // DFDL-935
  // @Test def test_encodingErrorPolicy_error() { runnerMB.runOneTest("encodingErrorPolicy_error") }
  // @Test def test_t2() { runnerMB.runOneTest("t2") }
  // @Test def test_t3() { runnerMB.runOneTest("t3") }
  @Test def test_t1(): Unit = { runnerMB.runOneTest("t1") }

  @Test def test_nested_group_refs1(): Unit = { runnerNG.runOneTest("nestedGroupRefs1") }

  @Test def test_AF000(): Unit = { runnerAF.runOneTest("AF000") }
  @Test def test_AF001(): Unit = { runnerAF.runOneTest("AF001") }
  @Test def test_AF002(): Unit = { runnerAF.runOneTest("AF002") }

  @Test def test_AG000(): Unit = { runnerAG.runOneTest("AG000") } // OK
  @Test def test_AG001(): Unit = { runnerAG.runOneTest("AG001") } // OK
  @Test def test_AG002(): Unit = { runnerAG.runOneTest("AG002") } // OK

  @Test def test_AW000(): Unit = { runnerAW.runOneTest("AW000") } // escape schemes
  @Test def test_AW001(): Unit = { runnerAW.runOneTest("AW001") }

  @Test def test_AY000(): Unit = { runnerAY.runOneTest("AY000") } // escape schemes

  @Test def test_AZ000(): Unit = { runnerAZ.runOneTest("AZ000") } // escape schemes

  // Jira DFDL-1392 - Issue with escapeEscape character that is first and precedes an escape-block start.
  // Is being removed, but should be preserved as it does not precede an escape character, nor an escape block end.
  //@Test def test_BA000() { runnerBA.runOneTest("BA000") } // escape schemes and delimiters
  //@Test def test_BB000() { runnerBB.runOneTest("BB000") } // occursCountKind stopValue

  //DFDL-1010
  @Test def test_BE000(): Unit = { runnerBE.runOneTest("BE000") } // unordered sequences
  @Test def test_BE001(): Unit = { runnerBE.runOneTest("BE001") }

  //DFDL-1010
  @Test def test_BF000(): Unit = { runnerBF1.runOneTest("BF000") } // unordered sequences
  @Test def test_BF001(): Unit = { runnerBF1.runOneTest("BF001") }

  @Test def test_BG000(): Unit = { runnerBG.runOneTest("BG000") }

  @Test def test_BC000(): Unit = { runnerBC.runOneTest("BC000") } // text boolean type
  @Test def test_BD000(): Unit = { runnerBD.runOneTest("BD000") } // binary boolean type

  // @Test def test_AP000() { runnerAP.runOneTest("AP000") } // lengthKind endOfParent - DAFFODIL-567

}
