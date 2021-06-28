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

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestUnparseNegInfoset {
  val runner = Runner("/org/apache/daffodil/unparser/", "unparseNegInfosetTest.tdml", validateTDMLFile = false)
  @AfterClass def tearDown(): Unit = {
    runner.reset
  }
}

class TestUnparseNegInfoset {
  import TestUnparseNegInfoset._

  @Test def test_schemaElementRoot1Good(): Unit = { runner.runOneTest("schemaElementRoot1Good") }
  @Test def test_schemaElementRoot2Good(): Unit = { runner.runOneTest("schemaElementRoot2Good") }

  @Test def test_unexpectedNextNone(): Unit = { runner.runOneTest("unexpectedNextNone") }
  @Test def test_unexpectedNextSingle(): Unit = { runner.runOneTest("unexpectedNextSingle") }
  @Test def test_unexpectedNextMultiple(): Unit = { runner.runOneTest("unexpectedNextMultiple") }

  @Test def test_uenxpectedChildNone(): Unit = { runner.runOneTest("unexpectedChildNone") }
  @Test def test_unexpectedChildSingle(): Unit = { runner.runOneTest("unexpectedChildSingle") }
  @Test def test_unexpectedChildMultiple(): Unit = { runner.runOneTest("unexpectedChildMultiple") }
  @Test def test_unexpectedChildSameAsSibling(): Unit = { runner.runOneTest("unexpectedChildSameAsSibling") }

  @Test def test_nilledTrueNonNillable(): Unit = { runner.runOneTest("nilledTrueNonNillable") }
  @Test def test_nilledFalseNonNillable(): Unit = { runner.runOneTest("nilledFalseNonNillable") }
  @Test def test_nilledSimpleWithContent(): Unit = { runner.runOneTest("nilledSimpleWithContent") }
  @Test def test_nilledComplexWithContent(): Unit = { runner.runOneTest("nilledComplexWithContent") }
  @Test def test_nilledBadValue(): Unit = { runner.runOneTest("nilledBadValue") }
}
