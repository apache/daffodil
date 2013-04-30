package edu.illinois.ncsa.daffodil

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TresysTests2 {
  val testDir = "/test-suite/tresys-contributed/"

  // This test passes now. Left this here to cut/paste for running other tests.    
  //  val ai = testDir + "AI.tdml"
  //  lazy val runnerAI = new DFDLTestSuite(Misc.getRequiredResource(ai))
  //
  //  @Test def test_AI000() { runnerAI.runOneTest("AI000") }

  //  lazy val runnerMB = new DFDLTestSuite(Misc.getRequiredResource(testDir + "mixed-binary-text.tdml"))
  //
  //  @Test def test_codingErrorAction() { runnerMB.runOneTest("codingErrorAction") }

  //  val ab = testDir + "ABLargeData.tdml.dat"
  //  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))
  // Too slow to use in regression. So slow it's a bug.
  // @Test def test_AB007() { runnerAB.runOneTest("AB007") }

  val ae = testDir + "AE.tdml"
  lazy val runnerAE = new DFDLTestSuite(Misc.getRequiredResource(ae))
  @Test def test_AE000() { runnerAE.runOneTest("AE000") } // needs newVariableInstance 

  // Commented out as we have no plans to make this run yet,
  // and there is no check for recursion, so they die slowly with stack 
  // overflow
  //
  //  val ao = testDir + "AO.tdml"
  //  lazy val runnerAO = new DFDLTestSuite(Misc.getRequiredResource(ao))
  //  @Test def test_AO000() { runnerAO.runOneTest("AO000") } // Needs recursion. Not in DFDL v1.0 spec.
  //  @Test def test_AO001() { runnerAO.runOneTest("AO001") } // Perhaps as an extension to DFDL.
  //  @Test def test_AO002() { runnerAO.runOneTest("AO002") }
  //  @Test def test_AO003() { runnerAO.runOneTest("AO003") }
  //  @Test def test_AO004() { runnerAO.runOneTest("AO004") }

  val ap = testDir + "AP.tdml"
  lazy val runnerAP = new DFDLTestSuite(Misc.getRequiredResource(ap))
  @Test def test_AP000() { runnerAP.runOneTest("AP000") } // lengthKind endOfParent

  val at = testDir + "AT.tdml"
  lazy val runnerAT = new DFDLTestSuite(Misc.getRequiredResource(at))
  @Test def test_AT000() { runnerAT.runOneTest("AT000") } // needs newVariableInstance

  val au = testDir + "AU.tdml"
  lazy val runnerAU = new DFDLTestSuite(Misc.getRequiredResource(au))
  @Test def test_AU000() { runnerAU.runOneTest("AU000") } // packed and bcd 

  val av0 = testDir + "AV000.tdml"
  lazy val runnerAV000 = new DFDLTestSuite(Misc.getRequiredResource(av0))
  @Test def test_AV000() { runnerAV000.runOneTest("AV000") } // needs date

  val av1 = testDir + "AV001.tdml"
  lazy val runnerAV001 = new DFDLTestSuite(Misc.getRequiredResource(av1))
  @Test def test_AV001() { runnerAV001.runOneTest("AV001") } // needs date

  val av2 = testDir + "AV002.tdml"
  lazy val runnerAV002 = new DFDLTestSuite(Misc.getRequiredResource(av2))
  @Test def test_AV002() { runnerAV002.runOneTest("AV002") } // needs date

  val av3 = testDir + "AV003.tdml"
  lazy val runnerAV003 = new DFDLTestSuite(Misc.getRequiredResource(av3))
  @Test def test_AV003() { runnerAV003.runOneTest("AV003") } // needs date

  // AW should just be debugged. Uses escape schemes. The test might be wrong
  // or the implementation. But the feature is implemented.
  val aw = testDir + "AW.tdml"
  lazy val runnerAW = new DFDLTestSuite(Misc.getRequiredResource(aw))
  @Test def test_AW000() { runnerAW.runOneTest("AW000") } // escape schemes
  @Test def test_AW001() { runnerAW.runOneTest("AW001") }

  // AY should just be debugged. Uses escape schemes. The test might be wrong
  // or the implementation. But the feature is implemented.
  val ay = testDir + "AY.tdml"
  lazy val runnerAY = new DFDLTestSuite(Misc.getRequiredResource(ay))
  @Test def test_AY000() { runnerAY.runOneTest("AY000") } // escape schemes

  // AZ should just be debugged. Uses escape schemes. The test might be wrong
  // or the implementation. But the feature is implemented.
  val az = testDir + "AZ.tdml"
  lazy val runnerAZ = new DFDLTestSuite(Misc.getRequiredResource(az))
  @Test def test_AZ000() { runnerAZ.runOneTest("AZ000") } // escape schemes

  // BA should just be debugged. Uses escape schemes. The test might be wrong
  // or the implementation. But the feature is implemented.
  val ba = testDir + "BA.tdml"
  lazy val runnerBA = new DFDLTestSuite(Misc.getRequiredResource(ba))
  @Test def test_BA000() { runnerBA.runOneTest("BA000") } // escape schemes and delimiters

  val bb = testDir + "BB.tdml"
  lazy val runnerBB = new DFDLTestSuite(Misc.getRequiredResource(bb))
  @Test def test_BB000() { runnerBB.runOneTest("BB000") } // occursCountKind stopValue

  val bc = testDir + "BC.tdml"
  lazy val runnerBC = new DFDLTestSuite(Misc.getRequiredResource(bc))
  @Test def test_BC000() { runnerBC.runOneTest("BC000") } // text boolean type

  val bd = testDir + "BD.tdml"
  lazy val runnerBD = new DFDLTestSuite(Misc.getRequiredResource(bd))
  @Test def test_BD000() { runnerBD.runOneTest("BD000") } // binary boolean type

  val be = testDir + "BE.tdml"
  lazy val runnerBE = new DFDLTestSuite(Misc.getRequiredResource(be))
  @Test def test_BE000() { runnerBE.runOneTest("BE000") } // unordered sequences
  @Test def test_BE001() { runnerBE.runOneTest("BE001") }

  val bf = testDir + "BF.tdml"
  lazy val runnerBF1 = new DFDLTestSuite(Misc.getRequiredResource(bf))
  @Test def test_BF000() { runnerBF1.runOneTest("BF000") } // unordered sequences
  @Test def test_BF001() { runnerBF1.runOneTest("BF001") }

  val bg = testDir + "BG.tdml"
  lazy val runnerBG = new DFDLTestSuite(Misc.getRequiredResource(bg))
  @Test def test_BG000() { runnerBG.runOneTest("BG000") } // needs decimal type
}
