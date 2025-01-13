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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestChoice extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/choice.tdml"
}

object TestChoiceLengthExplicit extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/ChoiceLengthExplicit.tdml"
}

class TestChoice extends TdmlTests {
  val tdmlSuite = TestChoice

  @Test def choice_noBranch = test

  @Test def optionalChoice01 = test
  @Test def optionalChoice02 = test
  @Test def optionalChoice03 = test
  @Test def optionalChoice04 = test
  @Test def optionalChoice05 = test

  @Test def choiceOfGroupRefs = test

  @Test def basic = test
  @Test def choice2 = test
  @Test def choice3 = test
  @Test def choice4 = test

  @Test def choiceWithInitsAndTermsInt = test
  @Test def choiceWithInitsAndTermsStr = test
  @Test def choiceWithInitsAndTermsError = test
  @Test def choiceWithInitsAndTermsSeqInt = test
  @Test def choiceWithInitsAndTermsSeqStr = test
  @Test def nestedChoiceWithInitsAndTermsNestedInt = test
  @Test def nestedChoiceWithInitsAndTermsNestedStr = test
  @Test def nestedChoiceWithInitsAndTermsInt = test

  @Test def choiceWithSequence1 = test
  @Test def choiceWithSequence2 = test
  @Test def choiceWithChoiceInt = test
  @Test def choiceWithChoiceFloat = test
  @Test def choiceWithChoiceString = test
  @Test def choiceWithArrayInts = test
  @Test def choiceWithArrayFloats = test
  @Test def choiceWithArrayString = test

  @Test def choice5 = test
  @Test def choice6 = test
  @Test def choiceFail1 = test
  @Test def choiceDelim1 = test
  @Test def choiceDelim2 = test
  @Test def choiceDelimFloat = test
  @Test def choiceDelimString = test
  @Test def choiceDelimStringwSp = test
  @Test def choiceDelimInt = test

  @Test def nestedChoice1 = test

  @Test def nestedChoiceAllString = test
  @Test def nestedChoiceAllFloat = test
  @Test def nestedChoiceAllInt = test

  @Test def choiceInSequenceWithSeparators1 = test
  @Test def choiceInSequenceWithSeparators2 = test

  @Test def sequenceInChoiceInSequenceWithSeparators1 = test
  @Test def sequenceInChoiceInSequenceWithSeparators2 = test
  @Test def sequenceInChoiceInSequenceWithSameSeparators1 = test
  @Test def sequenceInChoiceInSequenceWithSameSeparators2 = test

  @Test def choice_minOccurs = test
  @Test def choice_maxOccurs = test

  @Test def choice_with_inputvaluecalc = test

  // DFDL-641
  @Test def direct_dispatch_01 = test
  @Test def direct_dispatch_02 = test
  @Test def direct_dispatch_03 = test
  @Test def direct_dispatch_04 = test
  @Test def direct_dispatch_05 = test
  @Test def direct_dispatch_06 = test
  @Test def direct_dispatch_07 = test
  @Test def direct_dispatch_08 = test
  @Test def direct_dispatch_09 = test
  @Test def direct_dispatch_10 = test
  @Test def direct_dispatch_11 = test
  @Test def direct_dispatch_12 = test
  @Test def direct_dispatch_13 = test
  @Test def direct_dispatch_14 = test
  @Test def direct_dispatch_15 = test
  @Test def direct_dispatch_16 = test
  @Test def direct_dispatch_17 = test

  // DAFFODIL-2562
  @Test def dispatch_group_choice = test

  @Test def dispatch_group_choice_2 = test
  @Test def dispatch_group_choice_3 = test
}

class TestChoiceLengthExplicit extends TdmlTests {
  val tdmlSuite = TestChoiceLengthExplicit

  @Test def explicit_01 = test
  @Test def explicit_02 = test
  @Test def explicit_03 = test
  @Test def explicit_04 = test
  @Test def explicit_05 = test
  @Test def explicit_06 = test
  @Test def explicit_07 = test
  @Test def explicit_08 = test
  @Test def explicit_09 = test

  @Test def explicit_multiple_choices = test

  @Test def explicit_unparse_01 = test
  @Test def explicit_unparse_02 = test
}
