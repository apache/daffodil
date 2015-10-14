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

package edu.illinois.ncsa.daffodil.section07.discriminators

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestDiscriminators {
  val testDir = "/edu/illinois/ncsa/daffodil/section07/discriminators/"
  val runner = Runner(testDir, "discriminator.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }

}

class TestDiscriminators {

  import TestDiscriminators._

  @Test def test_discriminatorGuidesChoice() { runner.runOneTest("discriminatorGuidesChoice") }
  @Test def test_discriminatorGuidesChoice2() { runner.runOneTest("discriminatorGuidesChoice2") }
  @Test def test_discriminatorGuidesChoice3() = {
    //LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    runner.runOneTest("discriminatorGuidesChoice3")
  }
  @Test def test_discriminatorGuidesChoice4() { runner.runOneTest("discriminatorGuidesChoice4") }
  @Test def test_discriminatorGuidesChoice5() { runner.runOneTest("discriminatorGuidesChoice5") }

  @Test def test_discrimPatternPass() { runner.runOneTest("discrimPatternPass") }
  @Test def test_discrimPatternFail() { runner.runOneTest("discrimPatternFail") }

  @Test def test_discrimPatternFail2() { runner.runOneTest("discrimPatternFail2") }
  @Test def test_discrimPatternFail3() { runner.runOneTest("discrimPatternFail3") }
  @Test def test_choiceBranchDiscrim() { runner.runOneTest("choiceBranchDiscrim") }
  @Test def test_unparseDiscrimIgnored() { runner.runOneTest("unparseDiscrimIgnored") }

  @Test def test_discrimInvalidSchema() { runner.runOneTest("discrimInvalidSchema") }
  @Test def test_discrimOnSimpleType() { runner.runOneTest("discrimOnSimpleType") }
  @Test def test_discrimOnGroupRef() { runner.runOneTest("discrimOnGroupRef") }
  @Test def test_discrimOnGroupRef2() { runner.runOneTest("discrimOnGroupRef2") }
  @Test def test_discrimOnElementRef() { runner.runOneTest("discrimOnElementRef") }
  @Test def test_choiceBranchDiscrimFail() = { runner.runOneTest("choiceBranchDiscrimFail") }

  @Test def test_discrimPatternMatch() = { runner.runOneTest("discrimPatternMatch") }
  @Test def test_discrimPatternNoMatch() = { runner.runOneTest("discrimPatternNoMatch") }

  @Test def test_discrimExpression_01() = { runner.runOneTest("discrimExpression_01") }
  @Test def test_discrimExpression_02() = { runner.runOneTest("discrimExpression_02") }
  @Test def test_discrimExpression_03() = { runner.runOneTest("discrimExpression_03") }
}
