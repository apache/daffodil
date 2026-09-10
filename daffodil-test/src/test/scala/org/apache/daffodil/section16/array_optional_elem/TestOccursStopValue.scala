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

package org.apache.daffodil.section16.array_optional_elem

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestOccursStopValue extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section16/array_optional_elem/occursStopValue.tdml"
}

class TestOccursStopValue extends TdmlTests {
  val tdmlSuite = TestOccursStopValue

  @Test def sv01 = test
  @Test def sv02 = test
  @Test def sv03 = test
  @Test def sv04 = test
  @Test def sv05 = test
  @Test def sv06 = test
  @Test def sv07 = test
  @Test def sv08 = test

  @Test def svErrNoStopValue = test
  @Test def svErrEmptyStopValue = test
  @Test def svErrInvalidStopValue = test
  @Test def svErrSecondStopValueInvalid = test
  @Test def svErrInvalidHexStopValue = test
  @Test def svErrComplexStopValue = test
  @Test def svErrNillableStopValue = test
  @Test def svErrUnorderedStopValue = test
  @Test def svErrDefaultStopValue = test
  @Test def svExceedsMaxOccurs = test
  @Test def svErrMissingAfter = test
  @Test def svUnparse = test
  @Test def svUnparseEmpty = test
}
