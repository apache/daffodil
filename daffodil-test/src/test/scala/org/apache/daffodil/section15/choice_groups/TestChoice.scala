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

  val runnerCH = Runner(testDir, "choice.tdml")
  val runnerCE = Runner(testDir, "ChoiceLengthExplicit.tdml")

  @AfterClass def shutDown: Unit = {
    runnerCH.reset
    runnerCE.reset
  }

}

class TestChoice {

  import TestChoice._

  @Test def test_choice_noBranch (): Unit = { runnerCH.runOneTest("choice_noBranch") }

  @Test def test_optionalChoice01(): Unit = { runnerCH.runOneTest("optionalChoice01") }
  @Test def test_optionalChoice02(): Unit = { runnerCH.runOneTest("optionalChoice02") }
  @Test def test_optionalChoice03(): Unit = { runnerCH.runOneTest("optionalChoice03") }
  @Test def test_optionalChoice04(): Unit = { runnerCH.runOneTest("optionalChoice04") }
  @Test def test_optionalChoice05(): Unit = { runnerCH.runOneTest("optionalChoice05") }

  @Test def test_choiceOfGroupRefs(): Unit = { runnerCH.runOneTest("choiceOfGroupRefs") }

  @Test def test_basic(): Unit = { runnerCH.runOneTest("basic") }
  @Test def test_choice2(): Unit = { runnerCH.runOneTest("choice2") }
  @Test def test_choice3(): Unit = { runnerCH.runOneTest("choice3") }
  @Test def test_choice4(): Unit = { runnerCH.runOneTest("choice4") }

  @Test def test_choiceWithInitsAndTermsInt(): Unit = { runnerCH.runOneTest("choiceWithInitsAndTermsInt") }
  @Test def test_choiceWithInitsAndTermsStr(): Unit = { runnerCH.runOneTest("choiceWithInitsAndTermsStr") }
  @Test def test_choiceWithInitsAndTermsError(): Unit = { runnerCH.runOneTest("choiceWithInitsAndTermsError") }
  @Test def test_choiceWithInitsAndTermsSeqInt(): Unit = { runnerCH.runOneTest("choiceWithInitsAndTermsSeqInt") }
  @Test def test_choiceWithInitsAndTermsSeqStr(): Unit = { runnerCH.runOneTest("choiceWithInitsAndTermsSeqStr") }
  @Test def test_nestedChoiceWithInitsAndTermsNestedInt(): Unit = { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsNestedInt") }
  @Test def test_nestedChoiceWithInitsAndTermsNestedStr(): Unit = { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsNestedStr") }
  @Test def test_nestedChoiceWithInitsAndTermsInt(): Unit = { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsInt") }

  @Test def test_choiceWithSequence1(): Unit = { runnerCH.runOneTest("choiceWithSequence1") }
  @Test def test_choiceWithSequence2(): Unit = { runnerCH.runOneTest("choiceWithSequence2") }
  @Test def test_choiceWithChoiceInt(): Unit = { runnerCH.runOneTest("choiceWithChoiceInt") }
  @Test def test_choiceWithChoiceFloat(): Unit = { runnerCH.runOneTest("choiceWithChoiceFloat") }
  @Test def test_choiceWithChoiceString(): Unit = { runnerCH.runOneTest("choiceWithChoiceString") }
  @Test def test_choiceWithArrayInts(): Unit = { runnerCH.runOneTest("choiceWithArrayInts") }
  @Test def test_choiceWithArrayFloats(): Unit = { runnerCH.runOneTest("choiceWithArrayFloats") }
  @Test def test_choiceWithArrayString(): Unit = { runnerCH.runOneTest("choiceWithChoiceString") }

  @Test def test_choice5(): Unit = { runnerCH.runOneTest("choice5") }
  @Test def test_choice6(): Unit = { runnerCH.runOneTest("choice6") }
  @Test def test_choiceFail1(): Unit = { runnerCH.runOneTest("choiceFail1") }
  @Test def test_choiceDelim1(): Unit = { runnerCH.runOneTest("choiceDelim1") }
  @Test def test_choiceDelim2(): Unit = { runnerCH.runOneTest("choiceDelim2") }
  @Test def test_choiceDelimFloat(): Unit = { runnerCH.runOneTest("choiceDelimFloat") }
  @Test def test_choiceDelimString(): Unit = { runnerCH.runOneTest("choiceDelimString") }
  @Test def test_choiceDelimStringwSp(): Unit = { runnerCH.runOneTest("choiceDelimStringwSp") }
  @Test def test_choiceDelimInt(): Unit = { runnerCH.runOneTest("choiceDelimInt") }

  @Test def test_nestedChoice1(): Unit = { runnerCH.runOneTest("nestedChoice1") }

  @Test def test_nestedChoiceAllString(): Unit = { runnerCH.runOneTest("nestedChoiceAllString") }
  @Test def test_nestedChoiceAllFloat(): Unit = { runnerCH.runOneTest("nestedChoiceAllFloat") }
  @Test def test_nestedChoiceAllInt(): Unit = { runnerCH.runOneTest("nestedChoiceAllInt") }

  @Test def test_choiceInSequenceWithSeparators1(): Unit = { runnerCH.runOneTest("choiceInSequenceWithSeparators1") }
  @Test def test_choiceInSequenceWithSeparators2(): Unit = { runnerCH.runOneTest("choiceInSequenceWithSeparators2") }

  @Test def test_sequenceInChoiceInSequenceWithSeparators1(): Unit = { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSeparators1") }
  @Test def test_sequenceInChoiceInSequenceWithSeparators2(): Unit = { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSeparators2") }
  @Test def test_sequenceInChoiceInSequenceWithSameSeparators1(): Unit = { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSameSeparators1") }
  @Test def test_sequenceInChoiceInSequenceWithSameSeparators2(): Unit = { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSameSeparators2") }

  @Test def test_choice_minOccurs(): Unit = { runnerCH.runOneTest("choice_minOccurs") }
  @Test def test_choice_maxOccurs(): Unit = { runnerCH.runOneTest("choice_maxOccurs") }

  @Test def test_choice_with_inputvaluecalc(): Unit = { runnerCH.runOneTest("choice_with_inputvaluecalc") }

  // DFDL-641
  @Test def test_direct_dispatch_01(): Unit = { runnerCH.runOneTest("direct_dispatch_01") }
  @Test def test_direct_dispatch_02(): Unit = { runnerCH.runOneTest("direct_dispatch_02") }
  @Test def test_direct_dispatch_03(): Unit = { runnerCH.runOneTest("direct_dispatch_03") }
  @Test def test_direct_dispatch_04(): Unit = { runnerCH.runOneTest("direct_dispatch_04") }
  @Test def test_direct_dispatch_05(): Unit = { runnerCH.runOneTest("direct_dispatch_05") }
  @Test def test_direct_dispatch_06(): Unit = { runnerCH.runOneTest("direct_dispatch_06") }
  @Test def test_direct_dispatch_07(): Unit = { runnerCH.runOneTest("direct_dispatch_07") }
  @Test def test_direct_dispatch_08(): Unit = { runnerCH.runOneTest("direct_dispatch_08") }
  @Test def test_direct_dispatch_09(): Unit = { runnerCH.runOneTest("direct_dispatch_09") }
  @Test def test_direct_dispatch_10(): Unit = { runnerCH.runOneTest("direct_dispatch_10") }
  @Test def test_direct_dispatch_11(): Unit = { runnerCH.runOneTest("direct_dispatch_11") }
  @Test def test_direct_dispatch_12(): Unit = { runnerCH.runOneTest("direct_dispatch_12") }
  @Test def test_direct_dispatch_13(): Unit = { runnerCH.runOneTest("direct_dispatch_13") }
  @Test def test_direct_dispatch_14(): Unit = { runnerCH.runOneTest("direct_dispatch_14") }
  @Test def test_direct_dispatch_15(): Unit = { runnerCH.runOneTest("direct_dispatch_15") }
  @Test def test_direct_dispatch_16(): Unit = { runnerCH.runOneTest("direct_dispatch_16") }
  @Test def test_direct_dispatch_17(): Unit = { runnerCH.runOneTest("direct_dispatch_17") }

  @Test def test_explicit_01(): Unit = { runnerCE.runOneTest("explicit_01") }
  @Test def test_explicit_02(): Unit = { runnerCE.runOneTest("explicit_02") }
  @Test def test_explicit_03(): Unit = { runnerCE.runOneTest("explicit_03") }
  @Test def test_explicit_04(): Unit = { runnerCE.runOneTest("explicit_04") }
  @Test def test_explicit_05(): Unit = { runnerCE.runOneTest("explicit_05") }
  @Test def test_explicit_06(): Unit = { runnerCE.runOneTest("explicit_06") }
  @Test def test_explicit_07(): Unit = { runnerCE.runOneTest("explicit_07") }
  @Test def test_explicit_08(): Unit = { runnerCE.runOneTest("explicit_08") }
  @Test def test_explicit_09(): Unit = { runnerCE.runOneTest("explicit_09") }

  @Test def test_explicit_multiple_choices(): Unit = { runnerCE.runOneTest("explicit_multiple_choices") }

  @Test def test_explicit_unparse_01(): Unit = { runnerCE.runOneTest("explicit_unparse_01") }
  @Test def test_explicit_unparse_02(): Unit = { runnerCE.runOneTest("explicit_unparse_02") }

}
