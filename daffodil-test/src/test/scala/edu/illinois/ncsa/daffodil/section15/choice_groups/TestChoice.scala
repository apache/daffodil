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

object TestChoice {
  val testDir = "/edu/illinois/ncsa/daffodil/section15/choice_groups/"
  val testDir1 = "/edu/illinois/ncsa/daffodil/ibm-tests/"

  val runnerCH = Runner(testDir, "choice.tdml")
  val runner = Runner(testDir1, "dpaext2.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerCH.reset
  }

}

class TestChoice {

  import TestChoice._

  @Test def test_optionalChoice01() { runnerCH.runOneTest("optionalChoice01") }
  @Test def test_optionalChoice02() { runnerCH.runOneTest("optionalChoice02") }
  @Test def test_optionalChoice03() { runnerCH.runOneTest("optionalChoice03") }
  @Test def test_optionalChoice04() { runnerCH.runOneTest("optionalChoice04") }
  @Test def test_optionalChoice05() { runnerCH.runOneTest("optionalChoice05") }

  @Test def test_choiceOfGroupRefs() { runnerCH.runOneTest("choiceOfGroupRefs") }

  @Test def test_basic() { runnerCH.runOneTest("basic") }
  @Test def test_choice2() { runnerCH.runOneTest("choice2") }
  @Test def test_choice3() { runnerCH.runOneTest("choice3") }
  @Test def test_choice4() { runnerCH.runOneTest("choice4") }

  @Test def test_choiceWithInitsAndTermsInt() { runnerCH.runOneTest("choiceWithInitsAndTermsInt") }
  @Test def test_choiceWithInitsAndTermsStr() { runnerCH.runOneTest("choiceWithInitsAndTermsStr") }
  @Test def test_choiceWithInitsAndTermsError() { runnerCH.runOneTest("choiceWithInitsAndTermsError") }
  @Test def test_choiceWithInitsAndTermsSeqInt() { runnerCH.runOneTest("choiceWithInitsAndTermsSeqInt") }
  @Test def test_choiceWithInitsAndTermsSeqStr() { runnerCH.runOneTest("choiceWithInitsAndTermsSeqStr") }
  @Test def test_nestedChoiceWithInitsAndTermsNestedInt() { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsNestedInt") }
  @Test def test_nestedChoiceWithInitsAndTermsNestedStr() { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsNestedStr") }
  @Test def test_nestedChoiceWithInitsAndTermsInt() { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsInt") }

  @Test def test_choiceWithSequence1() { runnerCH.runOneTest("choiceWithSequence1") }
  @Test def test_choiceWithSequence2() { runnerCH.runOneTest("choiceWithSequence2") }
  @Test def test_choiceWithChoiceInt() { runnerCH.runOneTest("choiceWithChoiceInt") }
  @Test def test_choiceWithChoiceFloat() { runnerCH.runOneTest("choiceWithChoiceFloat") }
  @Test def test_choiceWithChoiceString() { runnerCH.runOneTest("choiceWithChoiceString") }
  @Test def test_choiceWithArrayInts() { runnerCH.runOneTest("choiceWithArrayInts") }
  @Test def test_choiceWithArrayFloats() { runnerCH.runOneTest("choiceWithArrayFloats") }
  @Test def test_choiceWithArrayString() { runnerCH.runOneTest("choiceWithChoiceString") }

  @Test def test_choice5() { runnerCH.runOneTest("choice5") }
  @Test def test_choice6() { runnerCH.runOneTest("choice6") }
  @Test def test_choiceFail1() { runnerCH.runOneTest("choiceFail1") }
  @Test def test_choiceDelim1() {
    // Logging@Test defaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("choiceDelim1")
  }
  @Test def test_choiceDelim2() { runnerCH.runOneTest("choiceDelim2") }
  @Test def test_choiceDelimFloat() { runnerCH.runOneTest("choiceDelimFloat") }
  @Test def test_choiceDelimString() { runnerCH.runOneTest("choiceDelimString") }
  @Test def test_choiceDelimStringwSp() { runnerCH.runOneTest("choiceDelimStringwSp") }
  @Test def test_choiceDelimInt() { runnerCH.runOneTest("choiceDelimInt") }

  @Test def test_nestedChoice1() {
    // Logging@Test defaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("nestedChoice1")
  }

  @Test def test_nestedChoiceAllString() { runnerCH.runOneTest("nestedChoiceAllString") }
  @Test def test_nestedChoiceAllFloat() { runnerCH.runOneTest("nestedChoiceAllFloat") }
  @Test def test_nestedChoiceAllInt() { runnerCH.runOneTest("nestedChoiceAllInt") }

  @Test def test_choiceInSequenceWithSeparators1() { runnerCH.runOneTest("choiceInSequenceWithSeparators1") }
  @Test def test_choiceInSequenceWithSeparators2() { runnerCH.runOneTest("choiceInSequenceWithSeparators2") }

  @Test def test_sequenceInChoiceInSequenceWithSeparators1() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSeparators1") }
  @Test def test_sequenceInChoiceInSequenceWithSeparators2() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSeparators2") }
  @Test def test_sequenceInChoiceInSequenceWithSameSeparators1() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSameSeparators1") }
  @Test def test_sequenceInChoiceInSequenceWithSameSeparators2() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSameSeparators2") }

  @Test def test_choice_minOccurs() { runnerCH.runOneTest("choice_minOccurs") }
  @Test def test_choice_maxOccurs() { runnerCH.runOneTest("choice_maxOccurs") }

  @Test def test_choice_with_inputvaluecalc() { runnerCH.runOneTest("choice_with_inputvaluecalc") }

  @Test def test_choices_basic_15_01() { runner.runOneTest("choices_basic_15_01") }
  @Test def test_choices_basic_15_02() { runner.runOneTest("choices_basic_15_02") }
  @Test def test_choices_basic_15_03() { runner.runOneTest("choices_basic_15_03") }
}
