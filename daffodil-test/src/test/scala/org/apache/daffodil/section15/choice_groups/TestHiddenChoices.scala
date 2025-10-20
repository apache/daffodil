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
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestHiddenChoices extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/HiddenChoices.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestHiddenChoices extends TdmlTests {
  val tdmlSuite = TestHiddenChoices

  @Test def parseHiddenGroupRef = test
  @Test def parseRegularGroupRef = test
  @Test def parseSeqOfHiddenAndRegularRef = test
  @Test def parseNestedHiddenAndRegularRef = test
  @Test def parseNestedHiddenGroupRefs = test
  @Test def parseIVCHiddenGroupRefs = test

  @Test def unparseHiddenGroupRef = test
  @Test def unparseRegularGroupRef = test
  @Test def unparseSeqOfHiddenAndRegularRef = test
  @Test def unparseNestedHiddenAndRegularRef = test
  @Test def unparseNestedRegularAndHiddenRef = test
  @Test def unparseNestedHiddenGroupRefs = test
  @Test def noOVCinHiddenContext = test
  @Test def nestedNoOVCinHiddenContext = test
  @Test def unparseIVCinHiddenContext = test
}
