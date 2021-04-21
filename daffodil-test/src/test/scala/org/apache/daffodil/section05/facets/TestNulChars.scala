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
package org.apache.daffodil.section05.facets

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestNulChars {
  val runner = Runner("/org/apache/daffodil/section05/facets", "NulChars.tdml")

  @AfterClass def shutDown() = {
    runner.reset
  }
}

import TestNulChars._

class TestNulChars {

  // DAFFODIL-2363 &#xE000; (NUL replacement into XML) can't be used in pattern facet. With full validation.
  @Test def test_nulPattern1() = { runner.runOneTest("nulPattern1") }

  // DAFFODIL-2364 - infinite loop
  // @Test def test_nulPad1() = { runner.runOneTest("nulPad1") }
}
