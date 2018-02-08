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

package edu.illinois.ncsa.daffodil.section15.choice_groups

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestChoiceGroupInitiatedContent {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/section15/choice_groups/"
  val runner_01 = Runner(testDir_01, "ChoiceGroupInitiatedContent.tdml")

  @AfterClass def shutDown {
    runner_01.reset
  }

}

class TestChoiceGroupInitiatedContent {

  import TestChoiceGroupInitiatedContent._

  @Test def test_initiatedContentChoice1() { runner_01.runOneTest("initiatedContentChoice1") }
  @Test def test_initiatedContentChoice2() { runner_01.runOneTest("initiatedContentChoice2") }
  @Test def test_initiatedContentChoice3() { runner_01.runOneTest("initiatedContentChoice3") }
  @Test def test_initiatedContentChoice4() { runner_01.runOneTest("initiatedContentChoice4") }
  @Test def test_initiatedContentChoice5() { runner_01.runOneTest("initiatedContentChoice5") }
  @Test def test_initiatedContentChoice6() { runner_01.runOneTest("initiatedContentChoice6") }
  @Test def test_initiatedContentChoice7() { runner_01.runOneTest("initiatedContentChoice7") }
  @Test def test_initiatedContentChoice8() { runner_01.runOneTest("initiatedContentChoice8") }
  @Test def test_initiatedContentChoice9() { runner_01.runOneTest("initiatedContentChoice9") }
  @Test def test_initiatedContentChoice10() { runner_01.runOneTest("initiatedContentChoice10") }

  @Test def test_arrayOfChoice() { runner_01.runOneTest("arrayOfChoice") }
  @Test def test_arrayOfChoice2() { runner_01.runOneTest("arrayOfChoice2") }
  @Test def test_discriminatorNesting1() { runner_01.runOneTest("discriminatorNesting1") }
  @Test def test_discriminatorNesting2() { runner_01.runOneTest("discriminatorNesting2") }
  @Test def test_Lesson5_choice_state() { runner_01.runOneTest("Lesson5_choice_state") }
  @Test def test_Lesson5_choice_county() { runner_01.runOneTest("Lesson5_choice_county") }
  @Test def test_Lesson5_choice_province() { runner_01.runOneTest("Lesson5_choice_province") }

  @Test def test_unparse_initiatedContentChoice1() { runner_01.runOneTest("unparse_initiatedContentChoice1") }
}
