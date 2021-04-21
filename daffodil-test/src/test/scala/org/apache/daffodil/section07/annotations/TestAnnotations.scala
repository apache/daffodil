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

package org.apache.daffodil.section07.annotations

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAnnotations {
  val testDir = "/org/apache/daffodil/section07/annotations/"
  val runner = Runner(testDir, "annotations.tdml", validateTDMLFile = false)

  @AfterClass def tearDown(): Unit = {
    runner.reset
  }
}

class TestAnnotations {

  import TestAnnotations._

  @Test def test_annotationInElementPass(): Unit = { runner.runOneTest("annotationInElementPass") }
  @Test def test_annotationInElementFail(): Unit = { runner.runOneTest("annotationInElementFail") }

  //DAFFODIL-2142
  @Test def test_annotationInComplexTypeWarn(): Unit = { runner.runOneTest("annotationInComplexTypeWarn") }
  @Test def test_multipleAppsInfosWarn(): Unit = { runner.runOneTest("multipleAppsInfosWarn") }
  @Test def test_noAnnotationsInCTPass(): Unit = { runner.runOneTest("noAnnotationsInCTPass") }
  @Test def test_noDFDLAnnotationsInCTPass(): Unit = { runner.runOneTest("noAnnotationsInCTPass") }

}
