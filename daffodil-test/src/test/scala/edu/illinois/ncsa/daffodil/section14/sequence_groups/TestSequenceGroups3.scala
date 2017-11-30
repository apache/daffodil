/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section14.sequence_groups

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSequenceGroups3 {

  val testDir_01 = "/edu/illinois/ncsa/daffodil/section14/sequence_groups/"

  val runner_01 = Runner(testDir_01, "SequenceGroupDelimiters.tdml")
  val runner_02 = Runner(testDir_01, "SequenceGroup.tdml", validateTDMLFile = false)

  @AfterClass def shutDown {
    runner_01.reset
    runner_02.reset
  }

}

class TestSequenceGroups3 {
  import TestSequenceGroups3._

  @Test def test_lastElts() { runner_01.runOneTest("lastElts") }

  @Test def test_hiddenGroupSeqWithRequiredElements() { runner_02.runOneTest("hiddenGroupSeqWithRequiredElements") }
  @Test def test_hiddenGroupChoiceWithAllRequiredBranches() { runner_02.runOneTest("hiddenGroupChoiceWithAllRequiredBranches") }

  @Test def test_sequence_group_with_annotation_01() { runner_02.runOneTest("sequence_group_with_annotation_01") }
  @Test def test_choice_group_with_annotation_01() { runner_02.runOneTest("choice_group_with_annotation_01") }

}
