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

object TestChoiceGroupInitiatedContent extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section15/choice_groups/ChoiceGroupInitiatedContent.tdml"
}

class TestChoiceGroupInitiatedContent extends TdmlTests {
  val tdmlSuite = TestChoiceGroupInitiatedContent

  @Test def initiatedContentChoice1 = test
  @Test def initiatedContentChoice2 = test
  @Test def initiatedContentChoice3 = test
  @Test def initiatedContentChoice4 = test
  @Test def initiatedContentChoice5 = test
  @Test def initiatedContentChoice6 = test
  @Test def initiatedContentChoice7 = test
  @Test def initiatedContentChoice8 = test
  @Test def initiatedContentChoice9 = test
  @Test def initiatedContentChoice10 = test

  // Test for DAFFODIL-2143
  @Test def arrayOptionalChildDiscriminatesElementAndChoice1 = test

  @Test def fixedArrayInitiatedContentDiscriminatesChoice = test
  @Test def parsedArrayMin1InitiatedContentDiscriminatesChoice = test

  @Test def arrayOfChoice = test
  @Test def arrayOfChoice2 = test
  @Test def discriminatorNesting1 = test
  @Test def discriminatorNesting2 = test
  @Test def Lesson5_choice_state = test
  @Test def Lesson5_choice_county = test
  @Test def Lesson5_choice_province = test

  @Test def unparse_initiatedContentChoice1 = test
  @Test def initiatedContentNestedChoices1 = test
  @Test def initiatedContentNestedChoices2 = test
}
