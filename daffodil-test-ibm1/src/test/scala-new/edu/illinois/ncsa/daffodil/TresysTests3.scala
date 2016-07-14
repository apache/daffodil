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

import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.Test

object TresysTests3 {
  val testDir = "/test-suite/tresys-contributed/"

  lazy val runnerBF = Runner(testDir, "bitFlagExpression.tdml",
    compileAllTopLevel = true)

  lazy val runnerAB = Runner(testDir, "ABLargeData.tdml.dat",
    compileAllTopLevel = true)

  lazy val runnerAH = Runner(testDir, "AH.tdml", compileAllTopLevel = true)

  lazy val runnerAM = Runner(testDir, "AM.tdml", validateTDMLFile = true, validateDFDLSchemas = false,
    compileAllTopLevel = true)

  lazy val runnerBC = Runner(testDir, "BC.tdml")
  lazy val runnerBD = Runner(testDir, "BD.tdml")
}

class TresysTests3 {
  import TresysTests3._

  @Test def test_testNone() = { runnerBF.runOneTest("testNone") }
  @Test def test_testOne() { runnerBF.runOneTest("testOne") }
  @Test def test_testMany() { runnerBF.runOneTest("testMany") }

  // Runs, but it is too slow to use in regression tests
  //@Test def test_AB006() { runnerAB.runOneTest("AB006") }

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

  @Test def test_AM000() { runnerAM.runOneTest("AM000") }
  @Test def test_AM001() { runnerAM.runOneTest("AM001") }

  @Test def test_BC000() { runnerBC.runOneTest("BC000") } // text boolean type
  @Test def test_BD000() { runnerBD.runOneTest("BD000") } // binary boolean type

}
