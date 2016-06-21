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

package edu.illinois.ncsa.daffodil

import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import org.junit.Test

class TresysTests3 {
  val testDir = "/test-suite/tresys-contributed/"

  lazy val runnerBF = new DFDLTestSuite(Misc.getRequiredResource(testDir + "bitFlagExpression.tdml"),
    compileAllTopLevel = true)

  @Test def test_testNone() = { runnerBF.runOneTest("testNone") }
  @Test def test_testOne() { runnerBF.runOneTest("testOne") }
  @Test def test_testMany() { runnerBF.runOneTest("testMany") }

  val ab = testDir + "ABLargeData.tdml.dat"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab),
    compileAllTopLevel = true)
  // Runs, but it is too slow to use in regression tests
  @Test def test_AB006() { runnerAB.runOneTest("AB006") }

  val ag = testDir + "AG.tdml"
  lazy val runnerAG = new DFDLTestSuite(Misc.getRequiredResource(ag),
    compileAllTopLevel = true)
  @Test def test_AG000() { runnerAG.runOneTest("AG000") } // OK
  @Test def test_AG001() { runnerAG.runOneTest("AG001") } // OK
  @Test def test_AG002() { runnerAG.runOneTest("AG002") } // OK

  val ah = testDir + "AH.tdml"
  lazy val runnerAH = new DFDLTestSuite(Misc.getRequiredResource(ah),
    compileAllTopLevel = true)
  @Test def test_AH000() { runnerAH.runOneTest("AH000") }
  @Test def test_AH001() { runnerAH.runOneTest("AH001") }
  @Test def test_AH002() { runnerAH.runOneTest("AH002") }

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
  // OVC, and arrays can't have either, they cannot be in hidden groups. It is
  // not worth fixing this test to work at the moment.
  val am = testDir + "AM.tdml"
  lazy val runnerAM = new DFDLTestSuite(Misc.getRequiredResource(am), validateTDMLFile = true, validateDFDLSchemas = false,
    compileAllTopLevel = true)
  //@Test def test_AM000() { runnerAM.runOneTest("AM000") }
  //@Test def test_AM001() { runnerAM.runOneTest("AM001") }
}
