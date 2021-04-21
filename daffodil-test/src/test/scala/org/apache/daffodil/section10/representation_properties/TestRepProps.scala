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

package org.apache.daffodil.section10.representation_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestRepProps {
  val testDir = "/org/apache/daffodil/section10/representation_properties/"
  val runner = Runner(testDir, "RepProps.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestRepProps {

  import TestRepProps._

  @Test def test_repPropMissing(): Unit = { runner.runOneTest("repPropMissing") }
  @Test def test_repPropMissing2(): Unit = { runner.runOneTest("repPropMissing2") }
  @Test def test_repPropMissing3(): Unit = { runner.runOneTest("repPropMissing3") }

  @Test def test_hexBinary_01(): Unit = { runner.runOneTest("hexBinary_01") }

  //These tests are temporary - see DFDL-994
  @Test def test_temporaryDefaultProps_01(): Unit = { runner.runOneTest("temporaryDefaultProps_01") }
  @Test def test_temporaryDefaultProps_02(): Unit = { runner.runOneTest("temporaryDefaultProps_02") }

}
