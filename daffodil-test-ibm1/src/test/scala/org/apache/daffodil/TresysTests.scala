/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil

import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner

object TresysTests {

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger {
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name")
  // }

  val testDir = "/test-suite/tresys-contributed/"

  val delimited = testDir + "dpaext1.tdml"
  lazy val runnerDelimited = new DFDLTestSuite(Misc.getRequiredResource(delimited))

  val td = testDir + "multiple-diagnostics.tdml"
  lazy val runnerMD = new DFDLTestSuite(Misc.getRequiredResource(td), validateTDMLFile = true, validateDFDLSchemas = false)

  runnerMD.setCheckAllTopLevel(true)

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

  /* Very big test data files, so each is in its own TDML file */

  //  val ab7 = testDir + "ABLargeData.tdml.dat"
  //  lazy val runnerAB7 = new DFDLTestSuite(Misc.getRequiredResource(ab7))
  //  val ab8 = testDir + "AB008.tdml"
  //  lazy val runnerAB8 = new DFDLTestSuite(Misc.getRequiredResource(ab8))
  //  val ab9 = testDir + "AB009.tdml"
  //  lazy val runnerAB9 = new DFDLTestSuite(Misc.getRequiredResource(ab9))

  val rd = testDir + "runtime-diagnostics.tdml"
  lazy val runnerRD = new DFDLTestSuite(Misc.getRequiredResource(rd),
    validateTDMLFile = false)

  runnerRD.setCheckAllTopLevel(true)

  val sq = testDir + "sequence.tdml"
  lazy val runnerSQ = new DFDLTestSuite(Misc.getRequiredResource(sq))

  lazy val runnerMB = new DFDLTestSuite(Misc.getRequiredResource(testDir + "mixed-binary-text.tdml"))
  val runnerNG = new DFDLTestSuite(Misc.getRequiredResource(testDir + "nested_group_ref.tdml"))
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

  //  val ba = testDir + "BA.tdml"
  //  lazy val runnerBA = new DFDLTestSuite(Misc.getRequiredResource(ba))

  lazy val runnerBC = Runner(testDir, "BC.tdml")
  lazy val runnerBD = Runner(testDir, "BD.tdml")
}

class TresysTests {
  import TresysTests._

  @Test def test_length_delimited_12_03_controversial() { runnerDelimited.runOneTest("length_delimited_12_03_controversial") }

  @Test def test_AX000() = { runnerAX.runOneTest("AX000") } // escape schemes

  @Test def test_AV000() { runnerAV000.runOneTest("AV000") } // needs date

  @Test def test_AV001() { runnerAV001.runOneTest("AV001") } // needs date

  @Test def test_AV002() { runnerAV002.runOneTest("AV002") }

  @Test def test_AV003() { runnerAV003.runOneTest("AV003") } // needs date

  @Test def test_multiple_diagnostics1() {
    runnerMD.runOneTest("twoMissingTypeDefErrors")
  }
  @Test def test_multiple_diagnostics2() { runnerMD.runOneTest("manyErrors1") }
  @Test def test_multiple_diagnostics3() { // LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerMD.runOneTest("manyErrors2")
  }

  // not found. Debug later.
  // @Test def test_duplicateDefineFormatsOneSchema() { runnerMD.runOneTest("duplicateDefineFormatsOneSchema") }

  @Test def test_nested_separator_delimited_baseline() { runnerNSD.runOneTest("baseline") }
  @Test def test_nested_separator_delimited_basicNest() { runnerNSD.runOneTest("basicNest") }
  // Fails infinite loop
  // @Test def test_nested_separator_delimited_basicNest2() { runnerNSD.runOneTest("basicNest2")}

  // Fails, index out of bounds
  // @Test def test_nested_separator_delimited_nest1() { runnerNSD.runOneTest("nest1")}
  // Fails infinite loop
  // @Test def test_nested_separator_delimited_nest2() { runnerNSD.runOneTest("nest2")}
  // Fails infinite loop
  // @Test def test_nested_separator_delimited_nest3() { runnerNSD.runOneTest("nest3")}

  /* Very big test data files, so each is in its own TDML file */

  //  @Test def test_AB007() { runnerAB7.runOneTest("AB007") }
  //  @Test def test_AB008() { runnerAB8.runOneTest("AB008") }
  //  @Test def test_AB009() { runnerAB9.runOneTest("AB009") }

  @Test def test_runtime_diagnostics1() { runnerRD.runOneTest("PE1") }

  @Test def test_seq1() { runnerSQ.runOneTest("seq1") }

  // DFDL-935
  // @Test def test_t1() { runnerMB.runOneTest("t1") }
  // @Test def test_t2() { runnerMB.runOneTest("t2") }
  // @Test def test_t3() { runnerMB.runOneTest("t3") }
  // @Test def test_encodingErrorPolicy_error() { runnerMB.runOneTest("encodingErrorPolicy_error") }

  @Test def test_nested_group_refs() { runnerNG.runOneTest("nestedGroupRefs") }

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

  // Jira DFDL-1392
  //  @Test def test_BA000() { runnerBA.runOneTest("BA000") } // escape schemes and delimiters

  @Test def test_BC000() { runnerBC.runOneTest("BC000") } // text boolean type
  @Test def test_BD000() { runnerBD.runOneTest("BD000") } // binary boolean type
}
