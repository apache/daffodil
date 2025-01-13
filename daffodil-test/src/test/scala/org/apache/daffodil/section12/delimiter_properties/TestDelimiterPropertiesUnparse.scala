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

package org.apache.daffodil.section12.delimiter_properties

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestDelimiterPropertiesUnparse extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section12/delimiter_properties/DelimiterPropertiesUnparse.tdml"
}

class TestDelimiterPropertiesUnparse extends TdmlTests {
  val tdmlSuite = TestDelimiterPropertiesUnparse

  @Test def unparseSeparatorLeadingSpace = test

  // DFDL-1493, DFDL-1477
  @Ignore @Test def unparseMultipleInitiators04 = test
  @Ignore @Test def unparseMultipleInitiators06 = test

  @Test def unparseMultipleInitiators05 = test
  @Test def unparseMultipleTerminators03 = test

  @Test def unparseMultipleInitiators01 = test
  @Test def unparseMultipleInitiators02 = test
  @Test def unparseMultipleInitiators03 = test
  @Test def unparseMultipleInitiators07 = test

  @Test def unparseMultipleTerminators01 = test
  @Test def unparseMultipleTerminators02 = test
  @Test def unparseMultipleTerminators04 = test
  @Test def unparseMultipleTerminators05 = test

  @Test def unparseMultipleSeparators01 = test
  @Test def unparseMultipleSeparators02 = test
  @Test def unparseMultipleSeparators03 = test
}
