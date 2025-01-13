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

package org.apache.daffodil.section14.sequence_groups

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestSequenceGroupDelimiters extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section14/sequence_groups/SequenceGroupDelimiters.tdml"
}

object TestSequenceGroup extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section14/sequence_groups/SequenceGroup.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestSequenceGroupDelimiters extends TdmlTests {
  val tdmlSuite = TestSequenceGroupDelimiters

  @Test def SeqGrp_01 = test
  @Test def SeqGrp_02 = test
  @Test def SeqGrp_03 = test
  @Test def SeqGrp_04 = test
  @Test def prefix = test
  @Test def prefix_01 = test
  @Test def NumSeq_02 = test
  @Test def groupRefInheritProps = test
  @Test def sequenceWithinSequence = test
  @Test def delimitedByNextInitFail = test
  @Test def separatorSuppressionPolicy_never = test
  @Test def separatorSuppressionPolicy_never_optionalStringArray_1 = test
  @Test def separatorSuppressionPolicy_never_optionalStringArray_2 = test
  @Test def separatorSuppressionPolicy_never_optionalStringArray_3 = test
  @Test def separatorSuppressionPolicy_never_optionalIntArray_1 = test
  @Test def separatorSuppressionPolicy_never_optionalIntArray_2 = test
  @Test def separatorSuppressionPolicy_never_optionalIntArray_3 = test
  @Test def separatorSuppressionPolicy_never_optionalIntArray_4 = test
  // DAFFODIL-2171
  @Test def delimiterScanning_01 = test
  @Test def delimiterScanning_02 = test
  @Ignore @Test def delimiterScanning_03 = test
  @Test def lastElts = test
}

class TestSequenceGroup extends TdmlTests {
  val tdmlSuite = TestSequenceGroup

  // DAFFODIL-669
  @Ignore @Test def emptySequenceSDE = test
  @Test def nadaParser = test
  @Test def complexEmptyContent = test
  @Test def noContentComplexSDE = test
  @Test def noContentAnnotatedComplexSDE = test

  @Test def SeqGrp546 = test

  @Test def SeqGrp_05 = test

  @Test def hiddenGroup1 = test
  @Test def hiddenGroupSchemaFail = test
  @Test def hiddenGroupWithAssert = test
  @Test def hiddenGroupWithAssert2 = test
  @Test def hiddenGroupNested = test
  @Test def hiddenGroupNested2 = test
  @Test def nestedGroupRefs = test
  @Test def nestedGroupRefs2 = test
  @Test def nestedGroupRefs3 = test
  @Test def nestedGroupRefs4 = test
  @Test def nestedGroupRefs5 = test
  @Test def hiddenGroupChoice = test
  @Test def hiddenGroupChoice2 = test
  @Test def hiddenGroupIgnoredProps = test
  @Test def hiddenGroupAttributeNotation = test
  @Test def hiddenGroupElementNotation = test

  // DFDL-284
  @Ignore @Test def hiddenGroupLoop = test

  // DFDL-598(related to, but this test does not say this is fixed)
  @Test def hiddenGroupRefEmptyString = test
  @Test def hiddenGroupRefDoesNotExist = test

  @Test def AC000 = test
  @Test def AD000 = test
  @Test def AS000 = test

  @Test def noDefaultSeqKind = test
  @Test def sequenceWithComplexType = test

  @Test def hiddenGroupIVC = test

  // DAFFODIL-2736
  @Test def multipleElemSameName = test
  @Test def multipleElemSameNameDifferentNamespaces = test

  @Test def hiddenGroupSeqWithRequiredElements = test
  @Test def hiddenGroupChoiceWithAllRequiredBranches = test

  @Test def sequence_group_with_annotation_01 = test
  @Test def choice_group_with_annotation_01 = test

  @Test def similar_model_groups_01 = test
}
