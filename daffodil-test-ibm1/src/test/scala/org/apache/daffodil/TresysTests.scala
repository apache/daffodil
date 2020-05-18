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

import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.util.Misc
import org.junit.{ AfterClass, Test }
import org.apache.daffodil.tdml.Runner

object TresysTests {

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name")
  // }

  val testDir = "/test-suite/tresys-contributed/"

  lazy val runnerDelimited = Runner(testDir, "delimTests.tdml")

  lazy val runnerMD = Runner(testDir, "multiple-diagnostics.tdml", compileAllTopLevel = true)
  lazy val runnerMD_NV = Runner(testDir, "multiple-diagnostics.tdml", compileAllTopLevel = true, validateDFDLSchemas = false)
  
  val bb = testDir + "BB.tdml"
  lazy val runnerBB = new DFDLTestSuite(Misc.getRequiredResource(bb))
  val be = testDir + "BE.tdml"
  lazy val runnerBE = new DFDLTestSuite(Misc.getRequiredResource(be))
  val bf = testDir + "BF.tdml"
  lazy val runnerBF1 = new DFDLTestSuite(Misc.getRequiredResource(bf))
  val bg = testDir + "BG.tdml"
  lazy val runnerBG = new DFDLTestSuite(Misc.getRequiredResource(bg))
  val mb = testDir + "mixed-binary-text.tdml"
  lazy val runnerMB = new DFDLTestSuite(Misc.getRequiredResource(mb))

  val ap = testDir + "AP.tdml"
  lazy val runnerAP = new DFDLTestSuite(Misc.getRequiredResource(ap))
  val ax = testDir + "AX.tdml"
  lazy val runnerAX = new DFDLTestSuite(Misc.getRequiredResource(ax))
  val av0 = testDir + "AV000.tdml"
  lazy val runnerAV000 = new DFDLTestSuite(Misc.getRequiredResource(av0))
  val av1 = testDir + "AV001.tdml"
  lazy val runnerAV001 = new DFDLTestSuite(Misc.getRequiredResource(av1))
  val av2 = testDir + "AV002.tdml"
  lazy val runnerAV002 = new DFDLTestSuite(Misc.getRequiredResource(av2))
  val av3 = testDir + "AV003.tdml"
  lazy val runnerAV003 = new DFDLTestSuite(Misc.getRequiredResource(av3))
  val nsd = testDir + "nested-separator-delimited.tdml"
  lazy val runnerNSD = new DFDLTestSuite(Misc.getRequiredResource(nsd))


  lazy val runnerRD = Runner(testDir, "runtime-diagnostics.tdml", compileAllTopLevel = true, validateTDMLFile = false)

  val sq = testDir + "sequence.tdml"
  lazy val runnerSQ = new DFDLTestSuite(Misc.getRequiredResource(sq))

  lazy val runnerNG = new DFDLTestSuite(Misc.getRequiredResource(testDir + "nested_group_ref.tdml"))
  val af = testDir + "AF.tdml"
  lazy val runnerAF = new DFDLTestSuite(Misc.getRequiredResource(af))
  val ag = testDir + "AG.tdml"
  lazy val runnerAG = new DFDLTestSuite(Misc.getRequiredResource(ag))

  val aw = testDir + "AW.tdml"
  lazy val runnerAW = new DFDLTestSuite(Misc.getRequiredResource(aw))

  val ay = testDir + "AY.tdml"
  lazy val runnerAY = new DFDLTestSuite(Misc.getRequiredResource(ay))

  val az = testDir + "AZ.tdml"
  lazy val runnerAZ = new DFDLTestSuite(Misc.getRequiredResource(az))

  val ba = testDir + "BA.tdml"
  lazy val runnerBA = new DFDLTestSuite(Misc.getRequiredResource(ba))

  val bc = testDir + "BC.tdml"
  lazy val runnerBC = new DFDLTestSuite(Misc.getRequiredResource(bc))
  val bd = testDir + "BD.tdml"
  lazy val runnerBD = new DFDLTestSuite(Misc.getRequiredResource(bd))

  @AfterClass def shutDown {
    runnerDelimited.reset
    runnerMD.reset
    runnerMD_NV.reset
    runnerRD.reset
  }
}

class TresysTests {
  import TresysTests._

  @Test def test_length_delimited_12_03_controversial() { runnerDelimited.runOneTest("length_delimited_12_03_controversial") }

  @Test def test_AX000() = { runnerAX.runOneTest("AX000") } // escape schemes

  @Test def test_AV000() { runnerAV000.runOneTest("AV000") } // needs date

  @Test def test_AV001() { runnerAV001.runOneTest("AV001") } // needs date

  @Test def test_AV002() { runnerAV002.runOneTest("AV002") }

  @Test def test_AV003() { runnerAV003.runOneTest("AV003") } // needs date

  @Test def test_multiple_diagnostics1() { runnerMD.runOneTest("twoMissingTypeDefErrors") }
  @Test def test_multiple_diagnostics2() { runnerMD.runOneTest("manyErrors1") }
  @Test def test_multiple_diagnostics3() { runnerMD_NV.runOneTest("manyErrors2") }

  @Test def test_nested_separator_delimited_baseline() { runnerNSD.runOneTest("baseline") }

  // Fails in IBM DFDL - ambiguous separator/terminator not accepted.
  @Test def test_nested_separator_delimited_baseline_ibm() { runnerNSD.runOneTest("baseline_ibm") }

  @Test def test_nested_separator_delimited_basicNest() { runnerNSD.runOneTest("basicNest") }
  @Test def test_nested_separator_delimited_basicNest2() { runnerNSD.runOneTest("basicNest2")}

  // Nested delimiter issues - DAFFODIL-2101
  // @Test def test_nested_separator_delimited_nest1() { runnerNSD.runOneTest("nest1")}
  // Nested delimiter issues - DAFFODIL-2101
  // @Test def test_nested_separator_delimited_nest2() { runnerNSD.runOneTest("nest2")}
  @Test def test_nested_separator_delimited_nest3() { runnerNSD.runOneTest("nest3")}

  @Test def test_runtime_diagnostics1() { runnerRD.runOneTest("PE1") }

  @Test def test_seq1() { runnerSQ.runOneTest("seq1") }

  // DFDL-935
  // @Test def test_encodingErrorPolicy_error() { runnerMB.runOneTest("encodingErrorPolicy_error") }
  // @Test def test_t2() { runnerMB.runOneTest("t2") }
  // @Test def test_t3() { runnerMB.runOneTest("t3") }
  @Test def test_t1() { runnerMB.runOneTest("t1") }

  @Test def test_nested_group_refs1() { runnerNG.runOneTest("nestedGroupRefs1") }

  @Test def test_AF000() { runnerAF.runOneTest("AF000") }
  @Test def test_AF001() { runnerAF.runOneTest("AF001") }
  @Test def test_AF002() { runnerAF.runOneTest("AF002") }

  @Test def test_AG000() { runnerAG.runOneTest("AG000") } // OK
  @Test def test_AG001() { runnerAG.runOneTest("AG001") } // OK
  @Test def test_AG002() { runnerAG.runOneTest("AG002") } // OK

  @Test def test_AW000() { runnerAW.runOneTest("AW000") } // escape schemes
  @Test def test_AW001() { runnerAW.runOneTest("AW001") }

  @Test def test_AY000() { runnerAY.runOneTest("AY000") } // escape schemes

  @Test def test_AZ000() { runnerAZ.runOneTest("AZ000") } // escape schemes

  // Jira DFDL-1392 - Issue with escapeEscape character that is first and precedes an escape-block start.
  // Is being removed, but should be preserved as it does not precede an escape character, nor an escape block end.
  //@Test def test_BA000() { runnerBA.runOneTest("BA000") } // escape schemes and delimiters
  //@Test def test_BB000() { runnerBB.runOneTest("BB000") } // occursCountKind stopValue

  //DFDL-1010
  @Test def test_BE000() { runnerBE.runOneTest("BE000") } // unordered sequences
  @Test def test_BE001() { runnerBE.runOneTest("BE001") }

  //DFDL-1010
  @Test def test_BF000() { runnerBF1.runOneTest("BF000") } // unordered sequences
  @Test def test_BF001() { runnerBF1.runOneTest("BF001") }

  @Test def test_BG000() { runnerBG.runOneTest("BG000") }

  @Test def test_BC000() { runnerBC.runOneTest("BC000") } // text boolean type
  @Test def test_BD000() { runnerBD.runOneTest("BD000") } // binary boolean type

  // @Test def test_AP000() { runnerAP.runOneTest("AP000") } // lengthKind endOfParent - DAFFODIL-567

}
