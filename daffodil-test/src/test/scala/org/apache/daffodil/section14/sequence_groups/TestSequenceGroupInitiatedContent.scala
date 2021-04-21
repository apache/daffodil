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

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSequenceGroupInitiatedContent {
  val testDir_01 = "/org/apache/daffodil/section14/sequence_groups/"
  val runner_01 = Runner(testDir_01, "SequenceGroupInitiatedContent.tdml")

  @AfterClass def shutDown(): Unit = {
    runner_01.reset
  }

}

class TestSequenceGroupInitiatedContent {

  import TestSequenceGroupInitiatedContent._

  @Test def test_baseline(): Unit = { runner_01.runOneTest("initiatedContentSeqBaseline") }
  @Test def test_1(): Unit = { runner_01.runOneTest("initiatedContentSeq1") }
  @Test def test_2(): Unit = { runner_01.runOneTest("initiatedContentSeq2") }
  @Test def test_3(): Unit = { runner_01.runOneTest("initiatedContentSeq3") }

  // Tests for DAFFODIL-2143
  @Test def test_sequenceScalarChildDoesNotDiscriminateAnything1(): Unit = { runner_01.runOneTest("sequenceScalarChildDoesNotDiscriminateAnything1") }
  @Test def test_sequenceScalarChildDoesNotDiscriminateAnything2(): Unit = { runner_01.runOneTest("sequenceScalarChildDoesNotDiscriminateAnything2") }
  @Test def test_sequenceFixedOccursChildDoesNotDiscriminateAnything1(): Unit = { runner_01.runOneTest("sequenceFixedOccursChildDoesNotDiscriminateAnything1") }
  @Test def test_sequenceExpressionOccursChildDoesNotDiscriminateAnything1(): Unit = { runner_01.runOneTest("sequenceExpressionOccursChildDoesNotDiscriminateAnything1") }
  @Test def test_sequenceImplicitOccursLessThanMinOccursDoesNotDiscriminateAnything1(): Unit = { runner_01.runOneTest("sequenceImplicitOccursLessThanMinOccursDoesNotDiscriminateAnything1") }
  @Test def test_sequenceImplicitOccursZeroOrMoreDiscriminates1(): Unit = { runner_01.runOneTest("sequenceImplicitOccursZeroOrMoreDiscriminates1") }

}
