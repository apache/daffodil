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

package org.apache.daffodil.section05.simple_types

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestBlobs extends TdmlSuite {

  val tdmlResource = "/org/apache/daffodil/section05/simple_types/Blobs.tdml"
}

class TestBlobs extends TdmlTests {
  val tdmlSuite = TestBlobs

  @Test def blob_01 = test
  @Test def blob_01_insufficient = test
  @Test def blob_01_insufficient_complex = test
  @Test def blob_02 = test
  @Test def blob_03 = test
  @Test def blob_04 = test
  @Test def blob_05 = test
  @Test def blob_06 = test
  @Test def blob_07 = test
  @Test def blob_08 = test
  @Test def blob_09 = test
  @Test def blob_10 = test
  @Test def blob_11 = test
  @Test def blob_12 = test
  @Test def blob_13 = test
  @Test def blob_14 = test
  @Test def blob_15 = test

  @Test def blob_unparseError = test

  @Test def clob_01 = test
}
