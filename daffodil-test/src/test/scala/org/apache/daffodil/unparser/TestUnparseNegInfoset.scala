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

package org.apache.daffodil.unparser

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestUnparseNegInfoset extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/unparser/unparseNegInfosetTest.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestUnparseNegInfoset extends TdmlTests {
  val tdmlSuite = TestUnparseNegInfoset

  @Test def schemaElementRoot1Good = test
  @Test def schemaElementRoot2Good = test

  @Test def unexpectedNextNone = test
  @Test def unexpectedNextSingle = test
  @Test def unexpectedNextMultiple = test

  @Test def unexpectedChildNone = test
  @Test def unexpectedChildSingle = test
  @Test def unexpectedChildMultiple = test
  @Test def unexpectedChildSameAsSibling = test

  @Test def nilledTrueNonNillable = test
  @Test def nilledFalseNonNillable = test
  @Test def nilledSimpleWithContent = test
  @Test def nilledComplexWithContent = test
  @Test def nilledBadValue = test
}
