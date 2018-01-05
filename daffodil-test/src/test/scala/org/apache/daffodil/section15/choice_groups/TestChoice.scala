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

package org.apache.daffodil.section15.choice_groups

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestChoice {
  val testDir = "/org/apache/daffodil/section15/choice_groups/"
  val testDir1 = "/org/apache/daffodil/ibm-tests/"

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

  // DFDL-641
  @Test def test_direct_dispatch_01() { runnerCH.runOneTest("direct_dispatch_01") }
  @Test def test_direct_dispatch_02() { runnerCH.runOneTest("direct_dispatch_02") }
  @Test def test_direct_dispatch_03() { runnerCH.runOneTest("direct_dispatch_03") }
  @Test def test_direct_dispatch_04() { runnerCH.runOneTest("direct_dispatch_04") }
  @Test def test_direct_dispatch_05() { runnerCH.runOneTest("direct_dispatch_05") }
  @Test def test_direct_dispatch_06() { runnerCH.runOneTest("direct_dispatch_06") }
  @Test def test_direct_dispatch_07() { runnerCH.runOneTest("direct_dispatch_07") }
  @Test def test_direct_dispatch_08() { runnerCH.runOneTest("direct_dispatch_08") }
  @Test def test_direct_dispatch_09() { runnerCH.runOneTest("direct_dispatch_09") }
  @Test def test_direct_dispatch_10() { runnerCH.runOneTest("direct_dispatch_10") }
  @Test def test_direct_dispatch_11() { runnerCH.runOneTest("direct_dispatch_11") }
  @Test def test_direct_dispatch_12() { runnerCH.runOneTest("direct_dispatch_12") }
  @Test def test_direct_dispatch_13() { runnerCH.runOneTest("direct_dispatch_13") }
  @Test def test_direct_dispatch_14() { runnerCH.runOneTest("direct_dispatch_14") }
  @Test def test_direct_dispatch_15() { runnerCH.runOneTest("direct_dispatch_15") }
  @Test def test_direct_dispatch_16() { runnerCH.runOneTest("direct_dispatch_16") }
}
